{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}

module Main where

import           Language.Javascript.JSaddle.Warp
import           Reflex
import           Reflex.Dom hiding      (mainWidget, mainWidgetWithCss, mainWidgetWithHead, run)
import           Reflex.Dom.Core        (mainWidget, mainWidgetWithCss, mainWidgetWithHead)
import           Reflex.Dom.SemanticUI
import           Data.Int               (Int64)
import           Data.FileEmbed         (embedFile)
import           Data.Map               (Map)
import qualified Data.Map as M
import           Data.Maybe             (fromMaybe, fromJust, catMaybes)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            ((<>))
import           Data.Proxy             (Proxy(..))
import           Data.Text              (Text)
import qualified Data.Text as T
import           Data.Text.Encoding     (decodeUtf8)
import           Text.Read              (readMaybe)
import           Control.Applicative    ((<*>), (<$>))
import           Control.Monad          (void, forM, forM_)
import           Servant.API
import           Servant.Reflex
import           Common.Api
import           Common.Model           (Coffee(..), CoffeeId(..), Shot(..), ShotId(..))

data Tab
    = NewShot
    | ShowShots
    | NewBean
    deriving Eq

#ifdef __GHCJS__
main = mainWidgetInElementById "dialin-app" $ divClass "ui container" $ app
#else
main = run 3911 $ mainWidgetWithHead headElement $ divClass "ui container" $ app
#endif

getApiUrl
    :: MonadWidget t m
    => m (Dynamic t BaseUrl)
#ifdef __GHCJS__
getApiUrl = return $ constDyn $ BasePath "/api"
#else
getApiUrl = return $ constDyn $ BaseFullUrl Http "localhost" 8000 "/api"
#endif

-- TODO: Check for and handle errors.
app
    :: forall t m. MonadWidget t m
    => m ()
app = mdo
    (eNewShot, eShowShots, eNewBean) <- divClass "ui secondary pointing menu" $ do
        let elem = elDynAttr' "a"
        let staticAttrs = "class" =: "item"
        let dynLink' = dynLink elem staticAttrs curTab 
        eShowShots <- dynLink' ShowShots "Shots"
        eNewShot   <- dynLink' NewShot "Pull"
        eNewBean   <- dynLink' NewBean "Add Coffee"
        divClass "right menu" $ do
            elAttr "a" ("class" =: "item" <>
                        "href"  =: "/logout") $ text "Logout"
        return (eNewShot, eShowShots, eNewBean)
    curTab <- holdDyn NewShot $ leftmost [eNewShot, eShowShots, eNewBean]
    widgetHold newShotTab . leftmost $ [
          newShotTab   <$ eNewShot
        , shotsTab     <$ eShowShots
        , newBeanTab   <$ eNewBean
        ]
    return ()

#ifndef __GHCJS__
headElement
    :: MonadWidget t m
    => m ()
headElement = do
    el "title" $ text "DialIn"
    styleSheet $(embedFile "./../backend/static/app.css")
    script $(embedFile "./../backend/static/app.js")
    elAttr "meta" (M.singleton "charset" "urf-8") blank
    elAttr "meta" (M.fromList [
                        ("http-equiv", "X-UA-Compatible")
                      , ("content",    "IE=edge,chrome=1")
                    ]) blank
    elAttr "meta" (M.fromList [
                        ("name",    "viewport")
                      , ("content", "width=device-width, initial-scale=1.0, maximum-scale=1.0")
                    ]) blank
    return ()
  where
    styleSheet bs = elAttr "style"  (M.singleton "type" "text/css") $ text . decodeUtf8 $ bs
    script     bs = elAttr "script" (M.singleton "type" "text/javascript") $ text . decodeUtf8 $ bs
#endif

dynLink
    :: (MonadWidget t m, Eq a)
    => (Dynamic t (Map Text Text) -> m () -> m (El t, ()))
    -> Map Text Text
    -> Dynamic t a
    -> a
    -> Text
    -> m (Event t a)
dynLink elem staticAttrs cur x label = do
    let isActive = (== x) <$> cur
    attrs <- makeActive isActive staticAttrs
    (e, _) <- elem attrs $ text label
    return (x <$ domEvent Click e)

makeActive
    :: MonadWidget t m
    => Dynamic t Bool
    -> Map Text Text
    -> m (Dynamic t (Map Text Text))
makeActive isActive attrs = return $ zipDynWith addActive isActive (constDyn attrs)
  where
    addActive True  attrs' = M.insertWith (\new old -> T.unwords [old, new]) (T.pack "class") (T.pack "active") attrs'
    addActive False attrs' = attrs'

shotsTab
    :: forall t m. MonadWidget t m
    => m ()
shotsTab = mdo
    apiUrl <- getApiUrl
    let (_ :<|> _ :<|> getShots :<|> _ :<|> _) = client (Proxy :: Proxy Api) (Proxy :: Proxy m) (Proxy :: Proxy ()) apiUrl
    pb <- getPostBuild
    shotsResponse :: Event t (ReqResult () [(Int64, Shot, Coffee)]) <- getShots pb
    shots :: Dynamic t [(Int64, Shot, Coffee)] <- foldDyn (++) [] (fmapMaybe reqSuccess shotsResponse)
    let errs = fmapMaybe reqFailure shotsResponse
    dynText =<< holdDyn "" errs
    -- TODO: Use DynamicList from reflex-dom-contrib to dynamically remove
    -- deleted items from the list.
    simpleList (reverse <$> shots) shotWidget
    return ()

shotWidget
    :: forall t m. MonadWidget t m
    => Dynamic t (Int64, Shot, Coffee)
    -> m ()
shotWidget shotCoffeeDyn = do
    let valueAttrs = ("class" =: "value" <> "style" =: "font-size: 16pt")
        labelAttrs = ("class" =: "label" <> "style" =: "font-size: 10pt")

    let shotIdDyn' = fmap (\(shotId, s, c) -> (ShotId shotId, (s, c))) shotCoffeeDyn
    let (shotIdDyn, shotDyn) = (fmap fst shotIdDyn', fmap snd shotIdDyn')

    apiUrl <- getApiUrl
    let (_ :<|> _ :<|> _ :<|> _ :<|> deleteShot) = client (Proxy :: Proxy Api) (Proxy :: Proxy m) (Proxy :: Proxy ()) apiUrl
    
    divClass "ui raised segment" $ do
        delete <- divClass "ui top attached label" $ do
            dynText $ (coffeeName . snd) <$> shotDyn
            divClass "detail" $ dynText $ (coffeeRoaster . snd) <$> shotDyn

            (e, _) <- elAttr' "a" ("style" =: "float: right; color: red;") $ do
                elClass "i" "delete icon" blank
                text "Delete"
            return (() <$ domEvent Click e)

        resp :: Event t (ReqResult () NoContent) <- deleteShot (Right <$> shotIdDyn) (() <$ delete)
        widgetHold blank (const deletedMsg <$> resp)

        divClass "ui four statistics" $ do
            divClass "ui mini statistic" $ do
                elAttr "div" valueAttrs $ do
                    dynText $ (T.pack . fromInt . shotDose . fst) <$> shotDyn
                elAttr "div" labelAttrs $ text "dose (g)"
            divClass "ui mini statistic" $ do
                elAttr "div" valueAttrs $ do
                    dynText $ (T.pack . fromInt . shotYield . fst) <$> shotDyn
                elAttr "div" labelAttrs $ text "yield (g)"
            divClass "ui mini statistic" $ do
                elAttr "div" valueAttrs $ do
                    dynText $ (T.pack . show . shotTime . fst) <$> shotDyn
                elAttr "div" labelAttrs $ text "time (s)"
            divClass "ui mini statistic" $ do
                elAttr "div" valueAttrs $ do
                    dynText $ (T.pack . show . shotTemp . fst) <$> shotDyn
                elAttr "div" labelAttrs $ text "temp (°C)"

        el "p" blank

        divClass "ui five statistics" $ do
            descriptorItems shotDyn
                [ ("Acidity",    shotAcidity)
                , ("Body",       shotBody)
                , ("Sweetness",  shotSweetness)
                , ("Aftertaste", shotAftertaste)
                , ("Bitterness", shotBitterness)
                ]

        showIf (not . T.null . shotGrind . fst) shotDyn $ do
            el "p" blank
            divClass "ui left pointing label" $ do
                text "Grind: "
                elAttr "span" ("style" =: "font-weight: normal") $ dynText $ (shotGrind . fst) <$> shotDyn

        showIf (not . T.null . shotNotes . fst) shotDyn $ do
            el "p" blank
            divClass "ui left pointing label" $ do
                text "Notes: "
                elAttr "span" ("style" =: "font-weight: normal") $ dynText $ (shotNotes . fst) <$> shotDyn
        return ()
  where        
    descriptorItems shotDyn xs = forM_ xs (\(name, f) -> descriptorItem name $ (T.pack . show . f . fst) <$> shotDyn)
    deletedMsg = do
        divClass "ui success message" $ do
            divClass "header" $ text "Your shot has been deleted."
            el "p" $ text "It will no longer be listed after you reload this page."


descriptorItem
    :: MonadWidget t m
    => Text
    -> Dynamic t Text
    -> m ()
descriptorItem name valDyn = do
    --divClass "item" $ do
    --    divClass "content" $ do
    --        divClass "header" $ elAttr "div" ("style" =: "font-size: 1.5rem; padding-bottom: 4pt") $ dynText val
    --        divClass "description" $ text name
    divClass "ui mini statistic" $ do
        elAttr "div" valueAttrs $ do
            dynText valDyn
        elAttr "div" labelAttrs $
            text name
  where
    valueAttrs = ("class" =: "value" <> "style" =: "font-size: 14pt")
    labelAttrs = ("class" =: "label" <> "style" =: "font-size: 6pt")

showIf
    :: MonadWidget t m
    => (a -> Bool)
    -> Dynamic t a
    -> m b
    -> m b
showIf f dy actions = do
    let attr = ffor dy $ mkAttr . f
    elDynAttr "div" attr actions
  where
    mkAttr True  = mempty
    mkAttr False = ("style" =: "display: none")

newShotTab
    :: forall t m. MonadWidget t m
    => m ()
newShotTab = mdo
    apiUrl <- getApiUrl
    let (getCoffees :<|> _ :<|> _ :<|> newShot :<|> _) = client (Proxy :: Proxy Api) (Proxy :: Proxy m) (Proxy :: Proxy ()) apiUrl
    pb <- getPostBuild
    coffeesResponse :: Event t (ReqResult () [(Coffee, CoffeeId)]) <- getCoffees pb
    coffees :: Dynamic t [(Coffee, CoffeeId)] <- foldDyn (++) [] (fmapMaybe reqSuccess coffeesResponse)

    rec let entries :: Dynamic t (Map (Maybe CoffeeId) (DropdownItemConfig m)) = makeEntries <$> coffees 
        let opts = [DOFSearch, DOFSelection, DOFFluid]
        coffee <- semUiDropdownWithItems "coffees-dropdown" opts Nothing entries mempty

    divClass "ui hidden divider" blank

    (dose, yield, time, temp) <- divClass "ui four column centered grid" $ do
        dose <- divClass "four column centered row" $ do
            numberSpinner 200 "Dose (g)"
        yield <- divClass "four column centered row" $ do
            numberSpinner 450 "Yield (g)"
        time <- divClass "four column centered row" $ do
            numberSpinner' 28 "Time (s)"
        temp <- divClass "four column centered row" $ do
            numberSpinner' 94 "Temp (C)"
        return (dose, yield, time, temp)

    divClass "ui hidden divider" blank

    subhead "Grind"
    grind <- uiTextInput (constDyn $ fluid def) $ def & attributes .~ constDyn ("placeholder" =: "Grind (optional)")

    divClass "ui hidden divider" blank

    let textDescriptor = descriptor (read . T.unpack) buttonLabels

    subhead "Acidity"
    acidity <- textDescriptor "5"
    subhead "Body"
    body <- textDescriptor "5"
    subhead "Sweetness"
    sweetness <- textDescriptor "5"
    subhead "Aftertaste"
    aftertaste <- textDescriptor "5"
    subhead "Bitterness"
    bitterness <- textDescriptor "5"

    divClass "ui hidden divider" blank

    subhead "Notes"
    notes <- uiTextInput (constDyn $ fluid def) $ def & attributes .~ constDyn ("placeholder" =: "Notes (optional)")

    divClass "ui hidden divider" blank
    
    -- TODO: `widgetHold` the Save button and switch it out for the message
    -- when it is clicked and the response has been received.
    widgetHold blank (const savedMsg <$> resp)
    saved <- uiButton (fluid <$> def) $ text "Save"

    divClass "ui hidden divider" blank

    -- TODO: Handle the "no coffee selected" case and failure response case.
    let dynShot = Shot <$> dose <*> yield <*> time <*> temp <*> value grind <*> value notes <*> acidity <*> body <*> sweetness <*> aftertaste <*> bitterness
    let dynReq = zipDynWith (\s c -> Right (s, fromJust c)) dynShot coffee
    resp :: Event t (ReqResult () ShotId) <- newShot dynReq (() <$ saved)
    return ()
  where
    buttonLabels = ["1", "2", "3", "4", "5", "6", "7", "8"]
    subhead txt = elAttr "h2" ("class" =: "ui sub header") $ text txt
    savedMsg = do
        divClass "ui success message" $ do
            divClass "header" $ text "Your shot was saved!"
            el "p" $ text "It will now be listed in the Shots tab."

descriptor
    :: MonadWidget t m
    => (Text -> b)
    -> [Text]
    -> Text
    -> m (Dynamic t b)
descriptor f labels init = do
    x' <- buttonGroup labels init
    return $ fmap f x'

buttonGroup
    :: MonadWidget t m
    => [Text]
    -> Text
    -> m (Dynamic t Text)
buttonGroup labels init = mdo
    let attrs = ("class" =: "ui button")
    buttons <- divClass "eight ui buttons" $ forM labels (\l -> dynLink (elDynAttr' "div") attrs cur l l)
    cur <- holdDyn init . leftmost $ zipWith (<$) labels buttons
    return cur

makeEntries
    :: forall t m. MonadWidget t m
    => [(Coffee, CoffeeId)]
    -> Map (Maybe CoffeeId) (DropdownItemConfig m)
makeEntries xs = M.fromList $ (Nothing, DropdownItemConfig "Select or search a coffee" $ blank) : (map (mapFst Just) $ makeEntry <$> xs)
  where
    mapFst :: (a -> c) -> (a, b) -> (c, b)
    mapFst f (x, y) = (f x, y)

makeEntry
    :: forall t m. MonadWidget t m
    => (Coffee, CoffeeId)
    -> (CoffeeId, DropdownItemConfig m)
makeEntry (Coffee name roaster, id) = (id, DropdownItemConfig (name `T.append` " (" `T.append` roaster `T.append` ")") $ do
    divClass "item" $ do
        text name
        divClass "ui left pointing horizontal label" $ text $ roaster)

note
    :: e
    -> Maybe a
    -> Either e a
note e = maybe (Left e) Right

newBeanTab
    :: forall t m. MonadWidget t m
    => m ()
newBeanTab = mdo
    apiUrl <- getApiUrl
    let (_ :<|> newCoffee :<|> _ :<|> _ :<|> _) = client (Proxy :: Proxy Api) (Proxy :: Proxy m) (Proxy :: Proxy ()) apiUrl
    (name, roaster, saved) <- divClass "ui text container" $ do
        name    <- uiTextInput (constDyn $ fluid def) $ def & attributes .~ constDyn ("placeholder" =: "Coffee Name")
        divClass "ui hidden divider" blank
        roaster <- uiTextInput (constDyn $ fluid def) $ def & attributes .~ constDyn ("placeholder" =: "Roaster Name")
        widgetHold (divClass "ui hidden divider" blank) (const savedMsg <$> saved)
        saved <- uiButton (fluid <$> def) $ text "Save"
        return (name, roaster, saved)
    let dynCoffee = Coffee <$> value name <*> value roaster
    let dynEitherCoffee = Right <$> dynCoffee
    resp :: Event t (ReqResult () CoffeeId) <- newCoffee dynEitherCoffee (() <$ saved)
    return ()
  where
    savedMsg = do
        divClass "ui success message" $ do
            divClass "header" $ text "Your new coffee was saved!"
            el "p" $ text "It will now be available in the Pull tab."

numberSpinner
    :: MonadWidget t m
    => Int
    -> Text
    -> m (Dynamic t Int)
numberSpinner init label = do
    rec let eChange = mergeWith (+)
                [ -1  <$ evDec1
                , -10 <$ evDec10
                ,  1  <$ evInc1
                ,  10 <$ evInc10
                ]
        let bVal = current $ _textInput_value num
        let eSetValue = attachWith' bVal eChange $ \val change ->
                            (T.pack . fromInt) ((+) change $ (fromStr . T.unpack) val)

        evDec10 <- uiButton (compact . circular . basic . (custom "icon") <$> def) $ do
            elClass "i" "angle double left icon" blank
        evDec1  <- uiButton (compact . circular . basic . (custom "icon") <$> def) $ do
            elClass "i" "angle left icon" blank
        num <- divClass "ui labeled input" $ do
            divClass "ui small label" $ text label
            textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_attributes .~ constDyn ("step" =: "0.1" <>
                                                                         "style" =: "width: 5.5em; margin-right: 2pt")
                               & textInputConfig_initialValue .~ (T.pack . fromInt) init
                               & textInputConfig_setValue .~ eSetValue
        evInc1  <- uiButton (compact . circular . basic . (custom "icon") <$> def) $ do
            elAttr "i" ("class" =: "angle right icon" <> "style" =: "position: relative; right: 0.2em") blank
        evInc10 <- uiButton (compact . circular . basic . (custom "icon") <$> def) $ do
            elAttr "i" ("class" =: "angle double right icon" <> "style" =: "position: relative; right: 0.2em") blank
    return . fmap (fromStr . T.unpack) $ (_textInput_value num)
  where
    attachWith' b e f = attachWith f b e

numberSpinner'
    :: MonadWidget t m
    => Int
    -> Text
    -> m (Dynamic t Int)
numberSpinner' init label = do
    rec let eChange = mergeWith (+)
                [ -1  <$ evDec1
                ,  1  <$ evInc1
                ]
        let bVal = current $ _textInput_value num
        let eSetValue = attachWith' bVal eChange $ \val change ->
                            (T.pack . show) ((+) change $ (read . T.unpack) val)

        evDec1  <- uiButton (compact . circular . basic . (custom "icon") <$> def) $ do
            elClass "i" "angle left icon" blank
        num <- divClass "ui labeled input" $ do
            divClass "ui small label" $ text label
            textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_attributes .~ constDyn ("step" =: "0.1" <>
                                                                         "style" =: "width: 5.5em; margin-right: 2pt")
                               & textInputConfig_initialValue .~ (T.pack . show) init
                               & textInputConfig_setValue .~ eSetValue
        evInc1  <- uiButton (compact . circular . basic . (custom "icon") <$> def) $ do
            elAttr "i" ("class" =: "angle right icon" <> "style" =: "position: relative; right: 0.2em") blank
    return . fmap (read . T.unpack) $ (_textInput_value num)
  where
    attachWith' b e f = attachWith f b e

fromInt
    :: Int
    -> String
fromInt i = show (i `div` 10) ++ "." ++ show (i `mod` 10)

fromStr
    :: String
    -> Int
fromStr s = g * 10 + mg
  where
    g  = read $ takeWhile (/= '.') s
    mg = read . mySafeTail $ dropWhile (/= '.') s
    mySafeTail xs = if xs == [] then "0" else tail xs
