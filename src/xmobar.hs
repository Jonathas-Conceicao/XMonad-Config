-- XMobar Config
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import JonathasConceicao.Xmobar
  ( ColorTheme(..), withColor
  , icon, xmobarXMonadCmd, draculaTheme
  , highAndLowParameters, highAndLowParametersI
  )
import Xmobar

main :: IO ()
main = xmobar config

curTheme = draculaTheme
myBgColor = background curTheme
myFgColor = foreground curTheme
myHiColor = high curTheme
-- myLoColor = low curTheme
-- myNmColor = normal curTheme
-- myIcColor = icons curTheme
myE0Color = extra0 curTheme

addHiLo = highAndLowParameters curTheme

addLoHi = highAndLowParametersI curTheme

config :: Config
config = defaultConfig
  { font = "Bitstream DejaVu Sans Mono Book Bold 12"
  , additionalFonts = []
  , borderColor = myBgColor
  , border = TopB
  , bgColor = myBgColor
  , fgColor = myFgColor
  , alpha = 105
  , position = Top
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "/home/jonathas/.xmonad/icons/"
  , allDesktops = True
  , overrideRedirect = True

  -- WeatherX uses ICAO code, remeber to update the template call too
  -- [ ("SBAR", "Aracaju - SE")
  -- , ("SBPK", "Pelotas - RS")
  -- , ("SBPA", "Porto Alegre - RS")
  -- , ("SABE", "Buenos Aires - Argentina")]

  , commands = [ Run $ WeatherX "SBPA"
                 [ ("clear", "Clear")
                 , ("sunny", "Sunny")
                 , ("mostly clear", "M-Clear")
                 , ("mostly sunny", "M-Sunny")
                 , ("partly sunny", "P-Sunny")
                 , ("fair", "Fair")
                 , ("cloudy", "Cloudy")
                 , ("overcast", "Overcast")
                 , ("partly cloudy", "P-Cloudy")
                 , ("mostly cloudy", "M-Cloudy")
                 , ("considerable cloudiness", "Raining")
                 ]
                 (["--template", "POA: <tempC>°C <skyConditionS> <rh>% <windKmh>km/h"]
                   ++ addHiLo "18" "25")
                 18000

               , Run $ Cpu
                 (["--template", icon "cpu.xbm" ++ " <total>%"]
                  ++ addHiLo "10" "80")
                 20

               , Run $ Memory
                 (["--template", icon "mem.xbm" ++ " <usedratio>%"]
                  ++ addHiLo "30" "80")
                 20

               , Run $ Swap
                 (["--template", "<usedratio>%"]
                  ++ addHiLo "10" "50")
                 20

               , Run $ Brightness
                 (addHiLo "9" "42" ++
                  [ "--template"
                  , xmobarXMonadCmd 1 "bright-reset"
                  $ xmobarXMonadCmd 4 "bright-up"
                  $ xmobarXMonadCmd 5 "bright-down"
                  $ icon "bright.xbm" ++ " <percent>%"
                  , "--", "-D", "intel_backlight"])
                 50

               , Run $ Volume
                 "default"
                 "Master"
                 ( addHiLo "10" "70" ++
                   [ "--template"
                   , xmobarXMonadCmd 1 "volume-toggle-mute"
                   $ xmobarXMonadCmd 4 "volume-up"
                   $ xmobarXMonadCmd 5 "volume-down"
                   $ "<status> <volume>%"
                   ,"--"
                   , "--onc",  myFgColor, "--on",  "<volumeipat>"
                   , "--offc", myFgColor, "--off", icon "sound_mute.xbm"
                   , "--volume-icon-pattern",      icon "sound_%%.xbm"
                   ])
                 20

               , Run $ Battery
                 ( ["--template", "<acstatus> <left>%"]
                 ++ addLoHi "10" "70"
                 ++ [ "--"
                    , "-O", icon "batt_on.xbm" ++ icon "batt.xbm"
                    , "-i", icon "batt_idle.xbm" ++ icon "batt.xbm"
                    , "-o", icon "batt.xbm"
                    ]
                 ) 600

               , Run $ DynNetwork
                 (["--template", (icon "wifi_8.xbm") ++ " <rx>*<tx>"]
                 -- ++ addHiLo "1" "50"
                 ) 20

               , Run $ DateZone "%A - %d %b(%m) %Y - %H:%M:%S" "" "" "date" 10
               , Run $ DateZone "%H:%M:%S" "" "Europe/London" "uk_time" 10

               , Run $ UnsafeStdinReader
               ]

  , sepChar = "%"
  , alignSep = "}{"
  , template = ""
      ++ xmobarXMonadCmd 1 "workspace-free" (icon "Fedora_Icon.xbm")
      ++ "| %UnsafeStdinReader% "
      ++ "} " ++ "%date%" `withColor` myE0Color
      ++ " (%uk_time%)" `withColor` myE0Color
      ++ "{ %SBPA% " -- Update this if SBPA is changed
      ++ "| %battery% "
      ++ "| %dynnetwork% "
      ++ "| %bright% "
      ++ "| %default:Master% "
      ++ "| %cpu% "
      ++ "| %memory% * %swap% "
      ++ "| " ++ xmobarXMonadCmd 1 "kill-focused" ((icon "skull.xbm") `withColor` myHiColor)
  }

