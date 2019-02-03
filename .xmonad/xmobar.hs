-- XMobar Config
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

import Xmobar
import JonathasConceicao.Xmobar
  ( ColorTheme(..), withColor
  , icon
  , draculaTheme, highAndLowParameters
  )

main :: IO ()
main = xmobar config


curTheme = draculaTheme
myBgColor = background curTheme
myFgColor = foreground curTheme
myHiColor = high curTheme
myLoColor = low curTheme
myNmColor = normal curTheme
myIcColor = icons curTheme
myE0Color = extra0 curTheme


addHiLo = highAndLowParameters curTheme

config :: Config
config = defaultConfig
  { font = "xft:Bitstream DejaVu Sans Mono Book:size=9:bold:antialias=true"
  , additionalFonts = []
  , borderColor = myBgColor
  , border = TopB
  , bgColor = myBgColor
  , fgColor = myFgColor
  , alpha = 255
  , position = Top
  , textOffset = -1
  , iconOffset = -1
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = ".xmonad/icons/" -- Path is relative to where XMobar is launched
  , allDesktops = True
  , overrideRedirect = True

  -- Weather uses ICAO code, remeber to update the template call too
  -- [("SBPK", "Pelotas - RS"), ("SBAR", "Aracaju - SE")]
  , commands = [ Run $ Weather "SBAR"
                 (["--template", "Aracaju: <tempC>°C"]
                  ++ addHiLo "18" "25")
                 36000

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
                 (["--template", icon "bright.xbm" ++ " <percent>%"]
                  ++ addHiLo "9" "42"
                  ++ ["--", "-D", "intel_backlight"])
                 50

               , Run $ Volume
                 "default"
                 "Master"
                 ([ "--template", "<status> <volume>%"]
                  ++ addHiLo "10" "70"
                  ++ ["--"
                     , "--onc",  myFgColor, "--on",  "<volumeipat>"
                     , "--offc", myFgColor, "--off", icon "sound_mute.xbm"
                     , "--volume-icon-pattern",      icon "sound_%%.xbm"
                     ])
                 20

               , Run $ Wireless "wlp3s0"
                 ( [ "--template", "<qualityipat>"]
                   ++ addHiLo "10" "70"
                   ++ ["--", "--quality-icon-pattern", icon "wifi_%%.xbm"])
                 20

               , Run $ Date "%A - %d %b(%m) %Y - %H:%M:%S" "date" 10

               , Run $ UnsafeStdinReader
               ]

  , sepChar = "%"
  , alignSep = "}{"
  , template = ""
      ++ icon "Fedora_Icon.xbm"
      ++ "| %UnsafeStdinReader% "
      ++ "} " ++ "%date%" `withColor` myE0Color
      ++ "{ %SBAR% "
      ++ "| %wlp3s0wi% "
      ++ "| %bright% "
      ++ "| %default:Master% "
      ++ "| %cpu% "
      ++ "| %memory% * %swap% "
      ++ "| " ++ (icon "skull.xbm") `withColor` myHiColor
  }

