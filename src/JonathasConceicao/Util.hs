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
  , commandsFromScriptsDir -- Creates a list of commands for all scripts in (xmonad path)/scripts
  -- commandsFromScriptsDir :: X [(String, X ())]
  )
  where

import XMonad (X, windows, asks, directories, cfgDir, spawn, liftIO)
import Control.Exception (IOException)
import System.Directory (getDirectoryContents)

econst :: Monad m => a -> IOException -> m a
econst = const . return

dummyStateUpdate :: X ()
dummyStateUpdate = windows id

xdotool :: String -> String
xdotool = (++) "xdotool key \\-\\-clearmodifiers "

hideString :: String -> String
hideString _ = ""

commandsFromScriptsDir :: X [(String, X ())]
commandsFromScriptsDir = do
  path <- flip (++) "/scripts/" <$> asks (cfgDir . directories)
  scripts <- liftIO $ getDirectoryContents path
  return [ (script, cmd)
         | script <- filter (/= ".")
                     $ filter (/= "..")
                     $ scripts
         , cmd <- pure $ execScript path script
         ]
  where
    execScript path script = spawn $ "'" ++ path ++ script ++ "'"
