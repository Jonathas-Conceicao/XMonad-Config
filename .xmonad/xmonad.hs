-- XMonad Config
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

import XMonad ( X(..), xmonad, def, (<+>)
              , terminal, modMask, mod4Mask, composeAll
              , startupHook, manageHook, layoutHook
              , logHook, handleEventHook )

import XMonad.Util.Run (spawnPipe, safeSpawn, hPutStrLn)
import XMonad.Util.EZConfig ( additionalKeysP )

import XMonad.Hooks.UrgencyHook (withUrgencyHook)
import XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                               , ppOutput, ppCurrent, ppVisible
                               , ppUrgent, ppHidden, ppLayout
                               , ppTitle, ppExtras, ppSep
                               , xmobarColor
                               , wrap
                               , shorten )
import XMonad.Hooks.ManageDocks ( Direction1D ( Next, Prev )
                                , docks, manageDocks, avoidStruts )
import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhDesktopsEventHook
                                 , fullscreenEventHook )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.NoBorders ( Ambiguity ( OnlyScreenFloat )
                               , lessBorders)
import XMonad.Actions.CycleWS ( WSType( NonEmptyWS, EmptyWS )
                              , nextScreen, shiftToNext
                              , prevScreen, shiftToPrev
                              , nextWS, prevWS, moveTo)

import JonathasConceicao.Volume
import JonathasConceicao.Brightness
import JonathasConceicao.Xmobar
import JonathasConceicao.Util
import JonathasConceicao.Notification
import JonathasConceicao.PlayerView

{- Main Config -}

main :: IO ()
main = do
  xmobarPipe <- spawnPipe "xmobar /home/?onathas/.xmonad/xmobarrc.config"
  xmonad
    $ docks
    $ withUrgencyHook LibNotifyUrgencyHook
    $ ewmh def
    { modMask = mod4Mask -- Use Super instead of Alt
    , startupHook = setWMName "LG3D"-- >> myStartupHook
    , manageHook = myManageHook
    , layoutHook = lessBorders OnlyScreenFloat $ avoidStruts  $  layoutHook def
    , logHook = dynamicLogWithPP $ myXMobarHook xmobarPipe
    , handleEventHook = myHandleEventHook
    , terminal = "/usr/bin/xterm"
    } `additionalKeysP` myKeys

{- My aliases -}

myXMobarHook pipe = def
  { ppOutput = hPutStrLn pipe
  , ppCurrent = xmobarColor "yellow" ""
                . xmobarAddAction (Just 1) (xdotool "Super_L+d")
                . xmobarAddAction (Just 3) (xdotool "Super_L+a")
                . wrap "[" "]"
  , ppVisible = xmobarColor "gray" "" . wrap "(" ")"
  , ppUrgent  = xmobarColor "red"  "" . wrap ">" "<"
  , ppHidden  = hideString
  , ppLayout = hideString
  , ppTitle   = xmobarColor "green"  "" . shorten 30
  , ppSep = " | "
  , ppExtras = [(formatVolume 10 70) getVolume]
  }

myHandleEventHook
  =   handleEventHook def
  <+> ewmhDesktopsEventHook
  <+> fullscreenEventHook

-- myStartupHook =

myManageHook = composeAll
  [ manageDocks
  , manageHook def
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

  -- Set window to Player View Mode
  , ("M-f", setToPlayerView)

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
