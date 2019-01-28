-- XMonad xmobar utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Xmobar
  (xmobarAddAction -- wrap Action to xmobar config
  -- xmobarAddAction :: Maybe Int -> String -> (String -> String)
  )
  where

import XMonad.Hooks.DynamicLog (wrap)

xmobarAddAction :: Maybe Int -> String -> (String -> String)
xmobarAddAction Nothing cmd =
  wrap
  ("<action=`" ++ cmd ++ "`>")
  "</action>"
xmobarAddAction (Just n) cmd =
  wrap
  ("<action=`" ++ cmd ++ "` button="++ (show n) ++">")
  "</action>"
