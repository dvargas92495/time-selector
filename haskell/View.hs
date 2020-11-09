{-# LANGUAGE MultiWayIf, OverloadedStrings, OverloadedLists #-}

module TimeSelector.View (view) where

import qualified Data.Text as T
import Data.Map.Strict
import Data.Time.Format

import Miso.String
import Miso.Html
import Miso.Event.Types
import TimeSelector.State
import Prelude hiding (length)
import Control.Lens.Indexed

bool :: a -> a -> Bool -> a
bool ifTrue ifFalse cond = if cond then ifTrue else ifFalse

(.~) :: Action -> v -> Action
a .~ v = \f -> f a v

view :: String -> State -> View Action
view k s = div_ []
    [ input_ [ autocomplete_ False
             , id_ . ms $ k <> "-input"
             , onInput $ \q -> SearchTimes q
             , onFocus . Modify $ sFocused .~ True
             , onKeyDown $ \c -> case c of
                    KeyCode 13 -> Select $ s ^. sIndex
                    KeyCode 27 -> Output Close
                    KeyCode 38 -> Modify $ sIndex .~ max 0 (s ^. sIndex - 1)
                    KeyCode 40 -> Modify $ sIndex .~ min (length (s ^. sResults) - 1)
                                                         (s ^. sIndex + 1)
                    KeyCode 192 -> Output Close
                    _ -> NoOp
             , placeholder_ "type a time and select below"
             , style_ inputStyle
             ]
    , if
        | s ^. sResults == [] -> div_ [style_ noResultStyle]
            [ span_ [] ["no results"]
            , span_ [style_ grayStyle] ["(try \"tomorrow\" or \"July 4, 2021 at 1:23pm\")"]
            ]
        | s ^. sInput == "" && not (s ^. sFocused) -> div_ [] []
        | otherwise -> div_ [] $ (itoList $ s ^. sResults) <&> \(i, (res, rt)) -> div_
            [ class_ "light-gray-background-on-hover"
            , onClick $ Select i
            , style_ . resultStyle $ s ^. sIndex == i
            ]
            [ p_ [] [text res]
            , div_ [style_ flexGrowStyle] []
            , p_ [style_ timeStyle]
                 [text . ms . T.toUpper . T.unwords . T.words
                       . T.pack $ formatTime defaultTimeLocale "%D %l:%M %p" rt
                 ]
            ]
    ]

flexGrowStyle :: Map MisoString MisoString
flexGrowStyle = [ ("flex-grow", "1") ]

grayStyle :: Map MisoString MisoString
grayStyle =
    [ ("color", "#888888")
    , ("margin-left", "10px")
    ]

inputStyle :: Map MisoString MisoString
inputStyle =
    [ ("border-left", "2px solid #27AAE1")
    , ("font-size", "16px")
    , ("padding", "10px 25px")
    , ("width", "100%")
    ]

noResultStyle :: Map MisoString MisoString
noResultStyle =
    [ ("align-items", "center")
    , ("display", "flex")
    , ("height", "50px")
    , ("padding", "0px 25px")
    ]

resultStyle :: Bool -> Map MisoString MisoString
resultStyle active =
    [ ("align-items", "center")
    , ("cursor", "pointer")
    , ("display", "flex")
    , ("height", "50px")
    , ("padding", "0px 25px")
    ] <> bool [] [("background-color", "#F0F0F0")] active

timeStyle :: Map MisoString MisoString
timeStyle =
    [ ("font-family", "monospace")
    , ("font-size", "14px")
    ]
