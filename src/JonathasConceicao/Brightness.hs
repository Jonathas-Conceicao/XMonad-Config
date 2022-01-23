-- XMonad screen brightness utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Brightness
  ( setBright --
   -- setBright :: (Int -> Int) -> X ()
  , resetBright
   -- resetBright :: X ()
  , lowerBright
   -- lowerBright :: Int -> X ()
  , raiseBright
   -- raiseBright :: Int -> X ()
  )
  where

import XMonad (X, liftIO)
import XMonad.Util.Run (safeSpawn)

import System.IO ( withFile
                 , IOMode(ReadWriteMode)
                 , Handle
                 , hPutStrLn
                 , hGetLine
                 )

resetBright :: X ()
resetBright = do
  safeSpawn "brightnessctl" ["set", "20"]
  safeSpawn "xmonad-display" ["Brightness"]


raiseBright :: Int -> X ()
raiseBright n = do
  safeSpawn "brightnessctl" ["set", "+" ++ (show n) ++ "%"]
  safeSpawn "xmonad-display" ["Brightness"]


lowerBright :: Int -> X ()
lowerBright n = do
  safeSpawn "brightnessctl" ["set", (show n) ++ "%" ++ "-"]
  safeSpawn "xmonad-display" ["Brightness"]

setBright :: (Int -> Int) -> X ()
setBright f = do
  liftIO $ withFile brightFile ReadWriteMode $ alterFile f
  safeSpawn "xmonad-display" ["Brightness"]

{- Private utility functions -}

brightFile :: FilePath
brightFile = "/sys/class/backlight/intel_backlight/brightness"

alterFile :: (Show a, Read a) => (a -> a) -> Handle -> IO ()
alterFile f h = do
  curr <- hGetLine h
  hPutStrLn h $ show $ f $ read curr
