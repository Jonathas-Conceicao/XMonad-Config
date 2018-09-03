import XMonad
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

import XMonad.Actions.CycleWS

import XMonad.Hooks.SetWMName

import System.IO

main :: IO ()
main = do
  xmobarPipe <- spawnPipe "xmobar /home/?onathas/.xmonad/xmobarrc.config"
  xmonad $ docks $ ewmh def
    { modMask = mod4Mask -- Use Super instead of Alt
    , startupHook = setWMName "LG3D"-- >> myStartupHook
    , manageHook = myManageHook
    , layoutHook = avoidStruts  $  layoutHook def
    , logHook = dynamicLogWithPP $ myXMobarHook xmobarPipe
    , handleEventHook = fullscreenEventHook
    , terminal = "/usr/bin/gnome-terminal"
    } `additionalKeysP` myKeys

myXMobarHook h = def
  { ppOutput = hPutStrLn h
  , ppCurrent = xmobarColor "yellow" ""
                . wrap "[" "]"
                . xmobarAddAction (Just 1) "xdotool key \\-\\-clearmodifiers Super_L+d"
                . xmobarAddAction (Just 3) "xdotool key \\-\\-clearmodifiers Super_L+a"
  , ppVisible = xmobarColor "gray"   "" . wrap "(" ")"
  , ppUrgent  = xmobarColor "red"    "" . wrap ">" "<"
  , ppHidden  = hideString

  , ppLayout = hideString

  , ppTitle   = xmobarColor "green"  "" . shorten 30
  , ppSep = " | "

  }
-- myStartupHook =

myManageHook = composeAll
  [ manageDocks
  , manageHook def
  , className =? "dota2" --> doIgnore
  ]

myKeys =
  [ ("M-x"  , safeSpawn "xmessage" ["\"Hello XMonad\""])

  , ("M-S-l", safeSpawn "dm-tool" ["lock"])

  -- Moving from Screen (for multiple monitors)
  , ("M-S-<Tab>", nextScreen)

  -- Moving from Workspace
  , ("M-d",       moveTo Next NonEmptyWS)
  , ("M-<Right>", moveTo Next NonEmptyWS)

  , ("M-a",      moveTo Prev NonEmptyWS)
  , ("M-<Left>", moveTo Prev NonEmptyWS)

  , ("M-S-d",       shiftToNext >> nextWS)
  , ("M-S-<Right>", shiftToNext >> nextWS)

  , ("M-S-a",      shiftToPrev >> prevWS)
  , ("M-S-<Left>", shiftToPrev >> prevWS)

  , ("M-n",        moveTo Next EmptyWS)

  -- Screenshot
  , ("<Print>"    , safeSpawn "gnome-screenshot" [])
  , ("S-<Print>"  , safeSpawn "gnome-screenshot" ["--area"])
  , ("M-<Print>"  , safeSpawn "gnome-screenshot" ["--window"])
  , ("C-S-<Print>", safeSpawn "gnome-screenshot" ["--window"])

  -- Audio control
  , ("<XF86AudioLowerVolume>", lowerVolume 3 >> return ())
  , ("M-S-<F2>"              , lowerVolume 3 >> return ())

  , ("<XF86AudioRaiseVolume>", raiseVolume 3 >> return ())
  , ("M-S-<F3>"              , raiseVolume 3 >> return ())

  , ("<XF86AudioMute>"       , toggleMute    >> return ())
  , ("M-S-<F4>"              , toggleMute    >> return ())

  -- Brightness control
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

lowerVolume :: Int -> X ()
lowerVolume n = unsafeSpawn $ "amixer -q sset Master " ++ (show n) ++ "%-"

raiseVolume :: Int -> X ()
raiseVolume n = unsafeSpawn $ "amixer -q sset Master " ++ (show n) ++ "%+"

toggleMute :: X ()
toggleMute = unsafeSpawn "amixer sset Master toggle"

brightFile :: FilePath
brightFile = "/sys/class/backlight/intel_backlight/brightness"

setBright :: (Int -> Int) -> X ()
setBright f = liftIO $ withFile brightFile ReadWriteMode $ alterFile f

alterFile :: (Show a, Read a) => (a -> a) -> Handle -> IO ()
alterFile f h = do
  curr <- hGetLine h
  hPutStrLn h $ show $ f $ read curr

hideString :: String -> String
hideString _ = ""

xmobarAddAction :: Maybe Int -> String -> (String -> String)
xmobarAddAction Nothing cmd = wrap ("<action=`" ++ cmd ++ "`>") "</action>"
xmobarAddAction (Just n) cmd = wrap ("<action=`" ++ cmd ++ "` button="++ (show n) ++">") "</action>"
