-- XMonad Config
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import XMonad ( xmonad, def, (<+>)
              , terminal, modMask, mod4Mask, composeAll
              , startupHook, manageHook, layoutHook
              , logHook, handleEventHook )
import XMonad.Operations (windows, sendMessage, kill)
import XMonad.StackSet (focusUp, focusDown, focusMaster)
import XMonad.Layout (ChangeLayout ( NextLayout ))

import XMonad.Util.Run (spawnPipe, safeSpawn, hPutStrLn)
import XMonad.Util.EZConfig ( additionalKeysP )

import XMonad.ManageHook (doFloat, className, stringProperty
                         , (-->), (=?) )
import XMonad.Hooks.Place (placeHook, simpleSmart)
import XMonad.Hooks.UrgencyHook (withUrgencyHook)
import XMonad.Hooks.DynamicLog ( dynamicLogWithPP
                               , ppOutput, ppCurrent, ppVisible
                               , ppUrgent, ppHidden, ppLayout
                               , ppTitle, ppSep
                               , xmobarColor
                               , wrap
                               , shorten )
import XMonad.Hooks.ManageDocks ( Direction1D ( Next, Prev )
                                , docks, manageDocks, avoidStruts )
import XMonad.Hooks.EwmhDesktops ( ewmh, ewmhFullscreen )
import XMonad.Hooks.ServerMode ( serverModeEventHookCmd' )
import XMonad.Layout.NoBorders ( Ambiguity ( OnlyScreenFloat )
                               , lessBorders )
import XMonad.Actions.CycleWS ( WSType( Not )
                              , emptyWS
                              , nextScreen, shiftToNext
                              , prevScreen, shiftToPrev
                              , nextWS, prevWS, moveTo)
import XMonad.Layout.Spacing ( Border(..), spacingRaw )

import XMonad.Prompt ( XPConfig, XPPosition (Top)
                     , font, position
                     , bgColor, fgColor
                     , bgHLight, fgHLight
                     , promptBorderWidth
                     , alwaysHighlight
                     , searchPredicate
                     , maxComplRows
                     )
import XMonad.Prompt.FuzzyMatch ( fuzzyMatch )
import XMonad.Prompt.XMonad ( xmonadPromptC )
import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )

import JonathasConceicao.Volume
import JonathasConceicao.Brightness
import JonathasConceicao.Xmobar
import JonathasConceicao.Util
import JonathasConceicao.Notification
import JonathasConceicao.PlayerView

{- Main Config -}

main :: IO ()
main = do
  xmobarPipe <- spawnPipe "xmobar"
  xmonad
    $ docks
    $ withUrgencyHook LibNotifyUrgencyHook
    $ ewmhFullscreen
    $ ewmh
    def
    { modMask = mod4Mask -- Use Super instead of Alt
    , startupHook = myStartupHook
    , manageHook = myManageHook <+> def manageHook
    , layoutHook = myLayoutHook
    , logHook = dynamicLogWithPP $ myXMobarHook xmobarPipe
    , handleEventHook = handleEventHook def <+> myHandleEventHook
    , terminal = "alacritty"
    } `additionalKeysP` myKeys

{- My aliases -}

myLayoutHook = spacingRaw False (Border 0 0 0 0) False (Border 5 5 5 5) True
  $ lessBorders OnlyScreenFloat
  $ avoidStruts
  $ layoutHook def

myXMobarHook pipe = def
  { ppOutput  = hPutStrLn pipe
  , ppCurrent = xmobarXMonadCmd 1 "workspace-next"
                . xmobarXMonadCmd 3 "workspace-prev"
                . xmobarColor "#F1FA8C" ""
                . wrap "[" "]"
  , ppVisible = xmobarXMonadCmd 1 "screen-next"
                . xmobarXMonadCmd 2 "screen-prev"
                . xmobarColor "#6272A4" "" . wrap "(" ")"
  , ppUrgent  = xmobarColor "#FF5555"  "" . wrap ">" "<"
  , ppHidden  = hideString
  , ppLayout  = xmobarXMonadCmd 1 "layout-next"
                . layoutIcons
  , ppTitle   = xmobarXMonadCmd 1 "window-next"
                . xmobarXMonadCmd 2 "window-master"
                . xmobarXMonadCmd 3 "window-prev"
                . xmobarColor "#BD93F9"  ""
                . shorten 70
  , ppSep     = " | "
  }


myHandleEventHook
  = (serverModeEventHookCmd' myXMonadCommands)

myStartupHook = do
  safeSpawn "picom"
    [ "--backend", "glx"
    , "--xrender-sync-fence"
    , "--fading", "--fade-delta=3"
    -- Opacity rules to set window transparency using WM_CLASS Property
    , "--opacity-rule", "90:class_g='Alacritty'"
    , "--opacity-rule", "90:class_g='Emacs'"
    , "--opacity-rule", "90:class_g='XTerm'"
    ]

  safeSpawn "dunst" []

myManageHook = composeAll
  [ placeHook simpleSmart
  , stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat
  , className =? "Xmessage" --> doFloat
  , manageDocks
  ]

myPrompt :: XPConfig
myPrompt = def
  { font = "xft:Bitstream DejaVu Sans Mono Book:size=9:bold:antialias=true"
  , bgColor = "#282A36"
  , fgColor = "#F8F8F2"
  , bgHLight = "#6272A4"
  , fgHLight = "#F8F8F2"
  , promptBorderWidth = 0
  , alwaysHighlight = True
  , position = Top
  , maxComplRows = Just 2
  , searchPredicate = fuzzyMatch
  }

confirm = confirmPrompt myPrompt

myXMonadPrompt = do
  list <- myXMonadCommands
  xmonadPromptC list myPrompt

myXMonadCommands = do
  let cmds = [ ("window-next", windows focusDown)
             , ("window-prev", windows focusUp)
             , ("window-master", windows focusMaster)
             , ("screen-next", nextScreen)
             , ("screen-prev", prevScreen)
             , ("workspace-next", moveTo Next $ Not emptyWS)
             , ("workspace-prev", moveTo Prev $ Not emptyWS)
             , ("workspace-free", moveTo Next emptyWS)
             , ("layout-next", sendMessage NextLayout)
             , ("togglePlayerView", togglePlayerView)

             , ("kill-focused", kill)

             , ("reboot", confirm "Reboot now?" $
                 safeSpawn "killall" ["--quit", "--ignore-case", "brave"] >>
                 safeSpawn "shutdown" ["--reboot", "0"])
             , ("poweroff", confirm "Poweroff now?" $
                 safeSpawn "killall" ["--quit", "--ignore-case", "brave"] >>
                 safeSpawn "shutdown" ["--poweroff", "0"])

             , ("volume-up", raiseVolume 3)
             , ("volume-down", lowerVolume 3)
             , ("volume-toggle-mute", toggleMute)

             , ("bright-up", raiseBright 3)
             , ("bright-down", lowerBright 3)
             , ("bright-reset", resetBright)
             ];
  scripts <- commandsFromScriptsDir
  return $ cmds ++ scripts

myKeys =
  [ ("M-x", myXMonadPrompt) -- Prompt for running XMonad commands

  -- , ("M-S-p", )
  -- Runs dmenu with my config
  , ("M-p", safeSpawn "dmenu_run" [ "-fn", "xft:Bitstream DejaVu Sans Mono Book:size=9:bold:antialias=true",
                                    "-p", "$>",
                                    "-nb", "#282A36",
                                    "-nf", "#F8F8F2",
                                    "-sb", "#6272A4",
                                    "-sf", "#F8F8F2"
                                  ])

  , ("M-S-l", safeSpawn "dm-tool" ["lock"])

  -- Moving from Screen (for multiple monitors)
  , ("M-S-<Tab>", nextScreen)

  -- Moving from Workspace
  , ("M-d",       moveTo Next $ Not emptyWS)
  , ("M-<Right>", moveTo Next $ Not emptyWS)

  , ("M-a",      moveTo Prev $ Not emptyWS)
  , ("M-<Left>", moveTo Prev $ Not emptyWS)

  , ("M-S-d",       shiftToNext >> nextWS)
  , ("M-S-<Right>", shiftToNext >> nextWS)

  , ("M-S-a",      shiftToPrev >> prevWS)
  , ("M-S-<Left>", shiftToPrev >> prevWS)

  , ("M-n",        moveTo Next emptyWS)

  -- Set window to Player View Mode
  , ("M-f", togglePlayerView)

  -- Screenshot
  , ("<Print>"    , safeSpawn "gnome-screenshot" [])
  , ("S-<Print>"  , safeSpawn "gnome-screenshot" ["--area"])
  , ("M-<Print>"  , safeSpawn "gnome-screenshot" ["--window"])
  , ("C-S-<Print>", safeSpawn "gnome-screenshot" ["--window"])

  -- Spotify media control
  , ("<XF86AudioPlay>", safeSpawn "playerctl" ["play-pause"])
  , ("<XF86AudioStop>", safeSpawn "playerctl" ["stop"])
  , ("<XF86AudioPrev>", safeSpawn "playerctl" ["prev"])
  , ("<XF86AudioNext>", safeSpawn "playerctl" ["next"])

  -- Audio control
  , ("<XF86AudioLowerVolume>", lowerVolume 3)
  , ("M-S-<F2>"              , lowerVolume 3)

  , ("<XF86AudioRaiseVolume>", raiseVolume 3)
  , ("M-S-<F3>"              , raiseVolume 3)

  , ("<XF86AudioMute>"       , toggleMute)
  , ("M-S-<F4>"              , toggleMute)

  -- Brightness control
  , ("<XF86MonBrightnessDown>", lowerBright 3)
  , ("<XF86Mail>"             , lowerBright 3)
  , ("M-S-<F9>"               , lowerBright 3)

  , ("<XF86MonBrightnessUp>"  , raiseBright 3)
  , ("<XF86HomePage>"         , raiseBright 3)
  , ("M-S-<F10>"              , raiseBright 3)

  , ("<XF86KbdLightOnOff>"    , resetBright)
  , ("<XF86Calculator>"       , resetBright)
  , ("M-S-<F12>"              , resetBright)

  -- dunst (notification service) control
  -- M1 denotates the Left Alt key
  , ("M-M1-S-<Space>", safeSpawn "dunstctl" ["close-all"])
  , ("M-M1-<Space>"  , safeSpawn "dunstctl" ["close"])
  , ("M-M1-h"        , safeSpawn "dunstctl" ["history-pop"])
  , ("M-M1-j"        , safeSpawn "dunstctl" ["context"])
  ]
