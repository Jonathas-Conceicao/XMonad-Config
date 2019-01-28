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
  , getVolume -- Logger with volume for state pretty-print
  -- getVolume :: Logger
  , formatVolume -- Add colors to low and high volume
  -- formatVolume :: Int -> Int -> Logger -> Logger
  )
  where

import JonathasConceicao.Util
  ( dummyStateUpdate
  , econst
  , xdotool
  )
import JonathasConceicao.Xmobar (xmobarAddAction)

import XMonad (X, liftIO)
import XMonad.Util.Loggers (Logger)
import XMonad.Util.Run (safeSpawn)
import XMonad.Hooks.DynamicLog (xmobarColor)

import Control.Exception (catch)
import System.Process (runInteractiveCommand)
import System.IO (hGetContents)

toggleMute :: X ()
toggleMute = do
  safeSpawn "amixer" ["sset", "Master", "toggle"]
  dummyStateUpdate

lowerVolume :: Int -> X ()
lowerVolume n = do
  safeSpawn "amixer" ["sset", "Master", (show n) ++ "%-", "-q"]
  dummyStateUpdate

raiseVolume :: Int -> X ()
raiseVolume n = do
  safeSpawn "amixer" ["sset", "Master", (show n) ++ "%+", "-q"]
  dummyStateUpdate

getVolume :: Logger
getVolume = liftIO $ do
  (_, out, _, _) <- runInteractiveCommand "amixer sget Master"
  mInfo <- hGetContents out `catch` econst ""
  return $ Just $ filterVolume mInfo

formatVolume :: Int -> Int -> Logger -> Logger
formatVolume lo hi l = do
  ms <- l
  case ms of
    Just s -> return $ Just $ rDye s
    Nothing -> l
  where
    rDye = ( xmobarAddAction (Just 1) (xdotool "Super_L+Shift_L+F3")
           . xmobarAddAction (Just 3) (xdotool "Super_L+Shift_L+F2")
           . xmobarAddAction (Just 2) (xdotool "Super_L+Shift_L+F4")
           . (++"%")
           . dye
           . read)
    dye x = if x < 0
            then show x
            else if x <= lo
                 then xmobarColor "lightblue" "" (show x)
                 else if x >= hi
                      then xmobarColor "red" "" (show x)
                      else xmobarColor "green" "" (show x)

{- Private utility functions -}

filterVolume :: String -> String
filterVolume = extract
  . dropWhile (/='[')
  . last
  . lines
  where
    extract [] = "-1"
    extract s = case status of
      "on" -> volume
      _    -> "-" ++ volume
      where
        info = getInfoInBrakets s
        volume = init $ head info
        status = last info

getInfoInBrakets :: String -> [String]
getInfoInBrakets [] = []
getInfoInBrakets s = case takeWhile (/=']') $ dropWhile (/='[') s of
  ""     -> []
  (_:ss) -> ss:(getInfoInBrakets $ tail $ dropWhile (/='[') s)
