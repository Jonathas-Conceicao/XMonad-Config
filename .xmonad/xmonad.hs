import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks

main = do
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc.hs"
  xmonad $ docks def
    { modMask = mod4Mask -- Use Super instead of Alt
    , manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts  $  layoutHook def
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                }
    , terminal = "xterm"
    } `additionalKeysP` myKeys
    

myKeys =
  [ ("M-x", spawn "xmessage 'Hello XMonad'")
  , ("M-S-l", spawn "dm-tool lock")
  ]
