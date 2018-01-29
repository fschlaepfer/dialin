{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Language.Javascript.JSaddle.Warp
import           Reflex
import           Reflex.Dom hiding      (mainWidget, mainWidgetWithCss, run)
import           Reflex.Dom.Core        (mainWidget, mainWidgetWithCss)
import           Data.FileEmbed
import           Data.Map               (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text              (pack, unpack, Text)
import           Text.Read              (readMaybe)
import           Control.Applicative    ((<*>), (<$>))
import           Control.Monad          (void)
import           Common.Foo

tabLink'
    :: MonadWidget t m
    => Text
    -> m (Event t ())
tabLink' t = do
    (e, _) <- elAttr' "a" ("class" =: "item") $ do -- TODO: keep track of current active tab in url? /app/addCoffee
        text t
    return $ domEvent Click e

{-
<div class="ui secondary pointing menu">
  <a class="item active">
    Home
  </a>
  <a class="item">
    Messages
  </a>
  <a class="item">
    Friends
  </a>
  <div class="right menu">
    <a class="ui item">
      Logout
    </a>
  </div>
</div>
<div class="ui segment">
  <p></p>
</div>
-}

tabLink
    :: MonadWidget t m
    => Text
    -> Event t () -- or Dynamic t Bool (isActive)
    -> m (Event t ())
tabLink t eActive = do
    
    (e, _) <- elDynAttr' "a" ("class" =: "item") $ do -- TODO: keep track of current active tab in url? /app/addCoffee
        text t
    return $ domEvent Click e

--main = run 3911 $ mainWidget $ el "div" $ do
--main = mainWidgetInElementById "dialin-app" $ el "div" $ do
main = run 3911 $ mainWidgetWithCss css $ divClass "ui text container" $ do
    (eNewShot, eShowShots, eNewBean) <- divClass "ui four item menu" $ do
        eNewShot   <- tabLink "Pull shot"
        eShowShots <- tabLink "My shots"
        eNewBean   <- tabLink "Add coffee"
        elAttr "a" ("class" =: "item" <>
                    "href"  =: "/logout" <>
                    "style" =: "color: red") $ text "Logout"
        return (eNewShot, eShowShots, eNewBean)
    widgetHold newShotTab . leftmost $ [
          newShotTab   <$ eNewShot
        , showShotsTab <$ eShowShots
        , newBeanTab   <$ eNewBean
        ]
    return ()
  where
    css = $(embedFile "./../backend/static/app.css")

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
showShotsTab = do
    text "View your shots here!"

newBeanTab
    :: MonadWidget t m
    => m ()
newBeanTab = do
    text "Add your new coffee here!"

uiButton
    :: MonadWidget t m
    => Text
    -> m (Event t ())
uiButton label = do
    (e, _) <- elClass' "button" "ui basic button" $ text label
    return $ domEvent Click e

-- | We represent a number with one decimal place in one of two ways:
-- * an Integer, where 16.5 corresponds to 165, or
-- * a String, where 16.5 corresponds to "16.5".
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
                            (pack . fromInt) ((+) change $ (fromStr . unpack) val)

        evDec10 <- uiButton "- 1.0"
        evDec1  <- uiButton "- 0.1"
        num <- divClass "ui labeled input" $ do
            divClass "ui label" $ text label
            textInput $ def & textInputConfig_inputType .~ "number"
                               & textInputConfig_attributes .~ constDyn ("step" =: "0.1" <> "style" =: "width: 5em")
                               & textInputConfig_initialValue .~ (pack . fromInt) init
                               & textInputConfig_setValue .~ eSetValue
        evInc1  <- uiButton "+ 0.1"
        evInc10 <- uiButton "+ 1.0"
    forDyn (_textInput_value num) (fromStr . unpack)
  where
    attachWith' b e f = attachWith f b e

{-
<div class="ui grid">
    <div class="four wide field">
        <dfLabel ref="dose">Dose [g]</dfLabel>
        <dfInput ref="dose" type="number" min="0" step="0.1" pattern="\d+" value="18" />
    </div>

    <div class="four wide field">
        <dfLabel ref="time">Time [s]</dfLabel>
        <dfInput ref="time" type="number" min="0" step="1" pattern="\d+" value="28" />
    </div>

    <div class="four wide field">
        <dfLabel ref="yield">Yield [g]</dfLabel>
        <dfInput ref="yield" type="number" min="0" step="0.1" pattern="\d+" value="36" />
    </div>

    <div class="four wide field">
        <dfLabel ref="temperature">Temp [°C]</dfLabel>
        <dfInput ref="temperature" type="number" min="0" step="1" pattern="\d+" value="92" />
    </div>
</div>
<br/>
-}



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
