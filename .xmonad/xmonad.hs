import XMonad
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Actions.CycleWS

main :: IO ()
main = do
  safeSpawnProg "$HOME/.xsession"
  xmonad $ docks def
      { modMask = mod4Mask -- Use Super instead of Alt
      , startupHook = myStartupHook
      , manageHook = manageDocks <+> manageHook def
      , layoutHook = avoidStruts  $  layoutHook def
      , terminal = "/usr/bin/gnome-terminal"
      } `additionalKeysP` myKeys

myStartupHook = do
  spawnOnce "xmobar $HOME/.xmonad/xmobarrc.hs"

myKeys =
  [ ("M-x", spawn "xmessage 'Hello XMonad'")
  , ("M-S-l", spawn "dm-tool lock")
  , ("M-S-<Tab>", nextScreen)
  ]
