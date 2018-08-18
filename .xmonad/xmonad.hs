{-# LANGUAGE CPP #-}
import XMonad
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS

#ifdef MIN_VERSION_xmonad_extras /* Any version will do it */
#define XMonadExtras
import XMonad.Actions.Volume
#endif

import System.IO

main :: IO ()
main = do
  xmonad $ docks $ ewmh def
    { modMask = mod4Mask -- Use Super instead of Alt
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , layoutHook = avoidStruts  $  layoutHook def
    , handleEventHook = fullscreenEventHook
    , terminal = "/usr/bin/gnome-terminal"
    } `additionalKeysP` myKeys

myStartupHook = do
  safeSpawn "xmobar" ["/home/?onathas/.xmonad/xmobarrc.hs"]

myManageHook = composeAll
  [ manageDocks
  , manageHook def
  ]

myKeys =
  [ ("M-x"  , safeSpawn "xmessage" ["\"Hello XMonad\""])

  , ("M-S-l", safeSpawn "dm-tool" ["lock"])

  , ("M-S-<Tab>", nextScreen)

  , ("<Print>"    , safeSpawn "gnome-screenshot" [])
  , ("S-<Print>"  , safeSpawn "gnome-screenshot" ["--area"])
  , ("M-<Print>"  , safeSpawn "gnome-screenshot" ["--window"])
  , ("C-S-<Print>", safeSpawn "gnome-screenshot" ["--window"])

#ifdef XMonadExtras
  , ("<XF86AudioLowerVolume>", lowerVolume 3 >> return ())
  , ("M-S-<F2>"              , lowerVolume 3 >> return ())

  , ("<XF86AudioRaiseVolume>", raiseVolume 3 >> return ())
  , ("M-S-<F3>"              , raiseVolume 3 >> return ())

  , ("<XF86AudioMute>"       , toggleMute    >> return ())
  , ("M-S-<F4>"              , toggleMute    >> return ())
#endif

  , ("<XF86MonBrightnessDown>", setBright (\x -> x - 50))
  , ("<XF86Mail>"             , setBright (\x -> x - 50))
  , ("M-S-<F9>"               , setBright (\x -> x - 50))

  , ("<XF86MonBrightnessUp>"  , setBright (\x -> x + 50))
  , ("<XF86HomePage>"         , setBright (\x -> x + 50))
  , ("M-S-<F10>"              , setBright (\x -> x + 50))

  , ("<XF86KbdLightOnOff>"    , setBright (\_ ->     20))
  , ("<XF86Calculator>"       , setBright (\_ ->     20))
  , ("M-S-<F12>"              , setBright (\_ ->     20))
  ]

brightFile :: FilePath
brightFile = "/sys/class/backlight/intel_backlight/brightness"

setBright :: (Int -> Int) -> X ()
setBright f = liftIO $ withFile brightFile ReadWriteMode $ alterFile f

alterFile :: (Show a, Read a) => (a -> a) -> Handle -> IO ()
alterFile f h = do
  curr <- hGetLine h
  hPutStrLn h $ show $ f $ read curr
