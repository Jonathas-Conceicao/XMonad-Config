-- XMonad xmobar utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Xmobar
  (xmobarAddAction -- wrap Action to xmobar config
  -- xmobarAddAction :: Maybe Int -> String -> (String -> String)
  , xmobarAddIcon-- Add the `Icon` before the `String`
  -- xmobarAddIcon :: Icon -> String -> String
  , layoutIcons -- Swap the Layout texts for the icons
  -- layoutIcons :: Logger -> Logger

  )
  where

import XMonad.Util.Loggers (Logger)
import XMonad.Hooks.DynamicLog (wrap)

-- Icons name on .xmonad/icons/ path
type Icon = String

xmobarAddAction :: Maybe Int -> String -> (String -> String)
xmobarAddAction Nothing cmd =
  wrap
  ("<action=`" ++ cmd ++ "`>")
  "</action>"
xmobarAddAction (Just n) cmd =
  wrap
  ("<action=`" ++ cmd ++ "` button="++ (show n) ++">")
  "</action>"

xmobarAddIcon :: Icon -> String -> String
xmobarAddIcon icon s = "<icon=/home/Jonathas/.xmonad/icons/"++ icon ++ "/>" ++ s

layoutIcons :: String -> String
layoutIcons "Tall"        = xmobarAddIcon "layout_tall.xbm" ""
layoutIcons "Mirror Tall" = xmobarAddIcon "layout_mirror.xbm" ""
layoutIcons "Full"        = xmobarAddIcon "layout_full.xbm" ""
layoutIcons _             = xmobarAddIcon "layout_err.xbm" ""
