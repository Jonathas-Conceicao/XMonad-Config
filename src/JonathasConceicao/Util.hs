-- XMonad general utility functions
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

module JonathasConceicao.Util
  ( econst
  -- econst :: Monad m => a -> IOException -> m a
  , dummyStateUpdate -- Send an id state update to xmonad
  --dummyStateUpdate :: X ()
  , xdotool -- Adds to string "xdotool --clearmodifiers" for xmobar
  -- xdotool :: String -> String
  , hideString -- Consumes string
  -- hideString :: String -> String
  , execScript -- Spawns script located at (xmonad path)/scripts
   -- execScript :: MonadIO m => String -> m ()
  )
  where

import XMonad (X, MonadIO, windows, getXMonadDir, spawn)
import Control.Exception (IOException)

econst :: Monad m => a -> IOException -> m a
econst = const . return

dummyStateUpdate :: X ()
dummyStateUpdate = windows id

xdotool :: String -> String
xdotool = (++) "xdotool key \\-\\-clearmodifiers "

hideString :: String -> String
hideString _ = ""

execScript :: MonadIO m => String -> m ()
execScript script = do
  xmonadDir <- getXMonadDir
  let path = xmonadDir ++ "/scripts/"
  spawn $ "'" ++ path ++ script ++ "'"
