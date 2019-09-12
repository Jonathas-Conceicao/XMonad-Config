-- XMonad function to position floating window on screen botom conner
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.PlayerView
  ( togglePlayerView -- Set current window to player view
  -- togglePlayerView :: X ()
  )
  where

import XMonad (X, Window, windows, windowset, gets, runQuery)
import XMonad.StackSet (RationalRect (..), float, sink, peek)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Hooks.FadeWindows (isFloating)

togglePlayerView :: X ()
togglePlayerView = do
  mw <- focusedWindow
  case mw of
    Just w -> do
      floating <- runQuery isFloating w
      if floating
        then (windows $ sink w) >> killAllOtherCopies
        else w `floatTo` rr >> windows copyToAll
    Nothing -> return ()
  where
    floatTo w ret = windows $ float w ret
    rr = RationalRect 0.65 0.65 0.33 0.33

{- Private utility functions -}

-- | Gets the currently focused window
focusedWindow :: X(Maybe Window)
focusedWindow = gets $ peek . windowset
