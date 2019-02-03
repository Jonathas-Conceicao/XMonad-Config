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
                               , ppTitle, ppSep
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
import XMonad.Prompt.Input ( inputPrompt, (?+) )
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
  xmobarPipe <- spawnPipe "xmobar /home/?onathas/.xmonad/xmobar.hs"
  xmonad
    $ docks
    $ withUrgencyHook LibNotifyUrgencyHook
    $ ewmh def
    { modMask = mod4Mask -- Use Super instead of Alt
    , startupHook = setWMName "LG3D"-- >> myStartupHook
    , manageHook = manageHook def <+> myManageHook 
    , layoutHook = lessBorders OnlyScreenFloat $ avoidStruts  $  layoutHook def
    , logHook = dynamicLogWithPP $ myXMobarHook xmobarPipe
    , handleEventHook = handleEventHook def <+> myHandleEventHook
    , terminal = "/usr/bin/xterm"
    } `additionalKeysP` myKeys

{- My aliases -}

myXMobarHook pipe = def
  { ppOutput  = hPutStrLn pipe
  , ppCurrent = xmobarColor "#F1FA8C" ""
                . wrap "[" "]"
  , ppVisible = xmobarColor "#6272A4" "" . wrap "(" ")"
  , ppUrgent  = xmobarColor "#FF5555"  "" . wrap ">" "<"
  , ppHidden  = hideString
  , ppLayout  = layoutIcons
  , ppTitle   = xmobarColor "#BD93F9"  ""
                . shorten 40
  , ppSep     = " | "
  }

myHandleEventHook
  =   ewmhDesktopsEventHook
  <+> fullscreenEventHook

-- myStartupHook =

myManageHook = composeAll
  [ manageDocks
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

myXMonadPrompt = xmonadPromptC
  [ ("next-wp", moveTo Next NonEmptyWS)
  , ("prev-wp", moveTo Prev NonEmptyWS)
  , ("togglePlayerView", togglePlayerView)
  , ("reboot", confirm "Reboot now?" $ safeSpawn "shutdown" ["--reboot", "0"])
  , ("poweroff", confirm "Poweroff now?" $ safeSpawn "shutdown" ["--poweroff", "0"])
  ] myPrompt

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
  , ("M-f", togglePlayerView)

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
