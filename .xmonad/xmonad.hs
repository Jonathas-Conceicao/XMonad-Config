import XMonad
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Actions.CycleWS
import XMonad.Actions.Volume

import System.IO

main :: IO ()
main = do
  safeSpawnProg "$HOME/.xsession"
  xmonad $ docks def
    { modMask = mod4Mask -- Use Super instead of Alt
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , layoutHook = avoidStruts  $  layoutHook def
    , terminal = "/usr/bin/gnome-terminal"
    } `additionalKeysP` myKeys

myStartupHook = do
  safeSpawnProg "xmobar $HOME/.xmonad/xmobarrc.hs"

myManageHook = composeAll
  [ manageDocks
  , isFullscreen --> doFullFloat
  , manageHook def
  ]

myKeys =
  [ ("M-x"  , safeSpawnProg "xmessage 'Hello XMonad'")
  , ("M-S-l", safeSpawnProg "dm-tool lock")

  , ("M-S-<Tab>", nextScreen)

  , ("<XF86AudioLowerVolume>", lowerVolume 3 >> return ())
  , ("<XF86AudioRaiseVolume>", raiseVolume 3 >> return ())
  , ("<XF86AudioMute>"       , toggleMute    >> return ())

  , ("<XF86MonBrightnessDown>", setBright (\x -> x - 50))
  , ("<XF86MonBrightnessUp>"  , setBright (\x -> x + 50))
  , ("<XF86KbdLightOnOff>"    , setBright (\_ ->     50))
  , ("<XF86HomePage>"         , setBright (\x -> x + 50))
  , ("<XF86Mail>"             , setBright (\x -> x - 50))
  ]

brightFile :: FilePath
brightFile = "/sys/class/backlight/intel_backlight/brightness"

setBright :: (Int -> Int) -> X ()
setBright f = liftIO $ withFile brightFile ReadWriteMode $ alterFile f

alterFile :: (Show a, Read a) => (a -> a) -> Handle -> IO ()
alterFile f h = do
  curr <- hGetLine h
  hPutStrLn h $ show $ f $ read curr
