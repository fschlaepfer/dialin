{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import           Language.Javascript.JSaddle.Warp
import           Reflex
import           Reflex.Dom hiding      (mainWidget, mainWidgetWithCss, mainWidgetWithHead, run)
import           Reflex.Dom.Core        (mainWidget, mainWidgetWithCss, mainWidgetWithHead)
import           Data.FileEmbed         (embedFile)
import           Data.Map               (Map)
import qualified Data.Map as M
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text as T
import           Data.Text.Encoding     (decodeUtf8)
import           Text.Read              (readMaybe)
import           Control.Applicative    ((<*>), (<$>))
import           Control.Monad          (void)
import           Common.Foo             (Foo(..))

data Tab
    = NewShot
    | ShowShots
    | NewBean
    deriving Eq

--main = run 3911 $ mainWidgetWithHead headElement $ divClass "ui text container" $ do
main = mainWidgetInElementById "dialin-app" $ el "div" $ do
    rec (eNewShot, eShowShots, eNewBean) <- divClass "ui secondary pointing menu" $ do
            eShowShots <- tabLink curTab ShowShots
            eNewShot   <- tabLink curTab NewShot
            eNewBean   <- tabLink curTab NewBean
            divClass "right menu" $ do
                elAttr "a" ("class" =: "item" <>
                            "href"  =: "/logout") $ text "Logout"
            return (eNewShot, eShowShots, eNewBean)
        curTab <- holdDyn NewShot $ leftmost [eNewShot, eShowShots, eNewBean]
        widgetHold newShotTab . leftmost $ [
              newShotTab   <$ eNewShot
            , showShotsTab <$ eShowShots
            , newBeanTab   <$ eNewBean
            ]
    return ()

headElement
    :: MonadWidget t m
    => m ()
headElement = do
    el "title" $ text "DialIn"
    styleSheet $(embedFile "./../backend/static/app.css")
    elAttr "meta" (M.singleton "charset" "urf-8") $ return ()
    elAttr "meta" (M.fromList [
                        ("http-equiv", "X-UA-Compatible")
                      , ("content",    "IE=edge,chrome=1")
                    ]) $ return ()
    elAttr "meta" (M.fromList [
                        ("name",    "viewport")
                      , ("content", "width=device-width, initial-scale=1.0, maximum-scale=1.0")
                    ]) $ return ()
    return ()
  where
    styleSheet bs = elAttr "style" (M.singleton "type" "text/css") $ text . decodeUtf8 $ bs


tabLink
    :: MonadWidget t m
    => Dynamic t Tab
    -> Tab
    -> m (Event t Tab)
tabLink cur t = do
    isActive <- return . fmap (== t) $ cur
    attrs <- makeActive isActive staticAttrs
    (e, _) <- elDynAttr' "a" attrs $ text $ tabName t
    return (t <$ domEvent Click e)
  where
    staticAttrs = "class" =: "item"
    tabName = \case
        NewShot   -> "Pull shot"
        ShowShots -> "View shots"
        NewBean   -> "Add coffee"

makeActive
    :: MonadWidget t m
    => Dynamic t Bool
    -> Map Text Text
    -> m (Dynamic t (Map Text Text))
makeActive isActive attrs = return $ zipDynWith addActive isActive (constDyn attrs)
  where
    addActive True  attrs' = M.insertWith (\new old -> T.unwords [old, new]) (T.pack "class") (T.pack "active") attrs'
    addActive False attrs' = attrs'

newShotTab
    :: MonadWidget t m
    => m ()
newShotTab = do
    divClass "ui four column centered grid" $ do
        divClass "four column centered row" $ do
            _ <- numberSpinner 155 "Dose"
            return ()
        divClass "four column centered row" $ do
            _ <- numberSpinner 250 "Time"
            return ()
        divClass "four column centered row" $ do
            _ <- numberSpinner 350 "Yield"
            return ()
    return ()

showShotsTab
    :: MonadWidget t m
    => m ()
showShotsTab = text "View your shots here!"

newBeanTab
    :: MonadWidget t m
    => m ()
newBeanTab = text "Add your new coffee here!"

uiButton
    :: MonadWidget t m
    => Text
    -> m (Event t ())
uiButton label = do
    (e, _) <- elClass' "button" "compact ui basic button" $ text label
    return $ domEvent Click e

numberSpinner
    :: MonadWidget t m
    => Integer
    -> Text
    -> m (Dynamic t Integer)
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

        evDec10 <- uiButton "--"
        evDec1  <- uiButton "-"
        num <- divClass "ui labeled input" $ do
            divClass "ui label" $ text label
            textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_attributes .~ constDyn ("step" =: "0.1" <>
                                                                         "style" =: "width: 5em")
                               & textInputConfig_initialValue .~ (T.pack . fromInt) init
                               & textInputConfig_setValue .~ eSetValue
        evInc1  <- uiButton "+"
        evInc10 <- uiButton "++"
    return . fmap (fromStr . T.unpack) $ (_textInput_value num)
  where
    attachWith' b e f = attachWith f b e

fromInt
    :: Integer
    -> String
fromInt i = show (i `div` 10) ++ "." ++ show (i `mod` 10)

fromStr
    :: String
    -> Integer
fromStr s = g * 10 + mg
  where
    g  = read $ takeWhile (/= '.') s
    mg = read . mySafeTail $ dropWhile (/= '.') s
    mySafeTail xs = if xs == [] then "0" else tail xs

testingFoo :: Foo
testingFoo = Foo "testFoo" 42
