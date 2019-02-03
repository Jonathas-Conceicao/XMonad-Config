-- XMonad xmobar utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Xmobar
  ( ColorTheme(..) -- Used by internal commands to set colors to config
  , draculaTheme -- Colors inspired by the Dracula Theme (https://github.com/dracula/dracula-theme)
  -- draculaTheme :: ColorTheme
  , xmobarAddAction -- wrap Action to xmobar config using xdotool
  -- xmobarAddAction :: Maybe Int -> String -> (String -> String)
  , icon-- Add the `Icon`
  -- icon :: Icon -> String
  , layoutIcons -- Swap the Layout texts for the icons
  -- layoutIcons :: Logger -> Logger
  , highAndLowParameters -- Returns parameters for XMobar config of High and Low values in the Theme Colors
  -- highAndLowParameters :: ColorTheme -> String -> String -> [String]
  , withColor -- Paints string
  -- withColor :: String -> Color -> String
  )
  where

import XMonad.Util.Loggers (Logger)
import XMonad.Hooks.DynamicLog (wrap)

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

xmobarAddAction :: Maybe Int -> String -> (String -> String)
xmobarAddAction Nothing cmd =
  wrap
  ("<action=`" ++ cmd ++ "`>")
  "</action>"
xmobarAddAction (Just n) cmd =
  wrap
  ("<action=`" ++ cmd ++ "` button="++ (show n) ++">")
  "</action>"

icon :: Icon -> String
icon icon = "<icon="++ icon ++ "/>"

layoutIcons :: String -> String
layoutIcons "Tall"        = icon "layout_tall.xbm"
layoutIcons "Mirror Tall" = icon "layout_mirror.xbm"
layoutIcons "Full"        = icon "layout_full.xbm"
layoutIcons _             = icon "layout_err.xbm"

highAndLowParameters :: ColorTheme -> String -> String -> [String]
highAndLowParameters colors lo hi =
  [ "-L", lo
  , "-H", hi
  , "--low"   , low colors
  , "--normal", normal colors
  , "--high"  , high colors 
  ]

withColor :: String -> Color -> String
withColor s c = "<fc="++c++">"++s++"</fc>"
