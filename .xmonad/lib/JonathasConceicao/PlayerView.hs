-- XMonad function to position floating window on screen botom conner
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.PlayerView
  ( setToPlayerView -- Set current window to player view
  -- setToPlayerView :: X ()
  )
  where

import XMonad (X, Window
              , windows
              , windowset
              , gets)
import XMonad.StackSet (RationalRect (..)
                       , float
                       , peek)

-- | Sets currently focused window to float at screen corner
setToPlayerView :: X ()
setToPlayerView = do
  mw <- focusedWindow
  case mw of
    Just w -> w `floatTo` rr
    Nothing -> return ()
  where
    floatTo w rr = windows $ float w rr
    rr = RationalRect 0.65 0.65 0.33 0.33

{- Private utility functions -}

-- | Gets the currently focused window
focusedWindow :: X(Maybe Window)
focusedWindow = gets (peek . windowset)
