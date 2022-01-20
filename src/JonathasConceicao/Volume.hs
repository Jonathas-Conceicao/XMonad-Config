-- XMonad audio utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Volume
  (toggleMute -- Toggle on/off mute on Master
  -- toggleMute :: X ()
  , lowerVolume -- Lower Master volume by n%
  -- lowerVolume :: Int -> X ()
  , raiseVolume -- Raise Master volume by n%
  -- raiseVolume :: Int -> X ()
  )
  where

import JonathasConceicao.Util (dummyStateUpdate)

import XMonad (X)
import XMonad.Util.Run (safeSpawn)

toggleMute :: X ()
toggleMute = do
  safeSpawn "amixer" ["sset", "Master", "toggle"]
  safeSpawn "xmonad-display" []
  dummyStateUpdate

lowerVolume :: Int -> X ()
lowerVolume n = do
  safeSpawn "amixer" ["sset", "Master", (show n) ++ "%-", "-q"]
  safeSpawn "xmonad-display" []
  dummyStateUpdate

raiseVolume :: Int -> X ()
raiseVolume n = do
  safeSpawn "amixer" ["sset", "Master", (show n) ++ "%+", "-q"]
  safeSpawn "xmonad-display" []
  dummyStateUpdate

