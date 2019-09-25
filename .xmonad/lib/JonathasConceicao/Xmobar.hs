-- XMonad xmobar utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Xmobar
  ( ColorTheme(..) -- Used by internal commands to set colors to config
  , draculaTheme -- Colors inspired by the Dracula Theme (https://github.com/dracula/dracula-theme)
  -- draculaTheme :: ColorTheme
  , xmobarAddAction -- wrap Action to xmobar config using xdotool
  -- xmobarAddAction :: Maybe Int -> String -> (String -> String)
  , xmobarXMonadCmd -- Calls xmobarAddAction with a xmonadctl argument
  -- xmobarXMonadCmd :: Maybe Int -> String -> (String -> String)
  , icon-- Add the `Icon`
  -- icon :: Icon -> String
  , layoutIcons -- Swap the Layout texts for the icons
  -- layoutIcons :: String -> String
  , highAndLowParameters -- Returns parameters for XMobar config of High and Low values in the Theme Colors
  -- highAndLowParameters :: ColorTheme -> String -> String -> [String]
  , highAndLowParametersI -- Returns parameters for XMobar config of High and Low values in the Theme Colors with low and high colors swaped
  -- highAndLowParameters :: ColorTheme -> String -> String -> [String]
  , withColor -- Paints string
  -- withColor :: String -> Color -> String
  )
  where

import XMonad.Hooks.DynamicLog (wrap)

import Data.List (isInfixOf)

-- Icons name on .xmonad/icons/ path
type Icon = String
-- Hex colors
type Color = String

data ColorTheme = ColorTheme
  { background :: Color
  , foreground :: Color
  , icons      :: Color
  , low        :: Color
  , normal     :: Color
  , high       :: Color
  , extra0     :: Color
  , extra1     :: Color
  , extra2     :: Color
  }

draculaTheme :: ColorTheme
draculaTheme =  ColorTheme
  { background = "#282A36"
  , foreground = "#F8F8F2"
  , icons      = "#F8F8F2"
  , low        = "#FF79C6"
  , normal     = "#BD93F9"
  , high       = "#FF5555"
  , extra0     = "#8BE9FD"
  , extra1     = "#50FA7B"
  , extra2     = "#F1FA8C"
  }

xmobarAddAction :: Int -> String -> String -> String
xmobarAddAction n cmd = wrap
  ("<action=`bash -c '" ++ cmd ++ "'` button="++ (show n) ++">")
  "</action>"

xmobarXMonadCmd :: Int -> String -> String -> String
xmobarXMonadCmd n cmd = xmobarAddAction n ("xmonadctl " ++ cmd)

icon :: Icon -> String
icon i = "<icon="++ i ++ "/>"

layoutIcons :: String -> String
layoutIcons s
  | "Mirror" `isInfixOf` s = icon "layout_mirror.xbm"
  | "Tall" `isInfixOf` s   = icon "layout_tall.xbm"
  | "Full" `isInfixOf` s   = icon "layout_full.xbm"
  | otherwise = icon "layout_err.xbm"

highAndLowParameters :: ColorTheme -> String -> String -> [String]
highAndLowParameters colors lo hi =
  [ "-L", lo
  , "-H", hi
  , "--low"   , low colors
  , "--normal", normal colors
  , "--high"  , high colors
  ]

highAndLowParametersI :: ColorTheme -> String -> String -> [String]
highAndLowParametersI colors lo hi =
  [ "-L", lo
  , "-H", hi
  , "--low"   , high colors
  , "--normal", normal colors
  , "--high"  , low colors
  ]

withColor :: String -> Color -> String
withColor s c = "<fc="++c++">"++s++"</fc>"
