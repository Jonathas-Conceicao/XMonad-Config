import XMonad hiding (float)

import qualified XMonad.StackSet as W

import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsEventHook, fullscreenEventHook)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders

import XMonad.Actions.CycleWS

import Control.Exception (catch, IOException)
import System.Process
import System.IO
import Data.List

import Data.Maybe (fromMaybe)

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

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

{- Utility Functions -}

econst :: Monad m => a -> IOException -> m a
econst = const . return

dummyStateUpdate :: X ()
dummyStateUpdate = windows id

toggleMute :: X ()
toggleMute = do
  safeSpawn "amixer" ["sset", "Master", "toggle"]
  dummyStateUpdate

lowerVolume :: Int -> X ()
lowerVolume n = do
  safeSpawn "amixer" ["sset", "Master", (show n) ++ "%-", "-q"]
  dummyStateUpdate

raiseVolume :: Int -> X ()
raiseVolume n = do
  safeSpawn "amixer" ["sset", "Master", (show n) ++ "%+", "-q"]
  dummyStateUpdate

getVolume :: Logger
getVolume = liftIO $ do
  (_, out, _, _) <- runInteractiveCommand "amixer sget Master"
  mInfo <- hGetContents out `catch` econst ""
  return $ Just $ filterVolume mInfo

filterVolume :: String -> String
filterVolume = extract
  . dropWhile (/='[')
  . last
  . lines
  where
    extract [] = "-1"
    extract s = case status of
      "on" -> volume
      _    -> "-" ++ volume
      where
        info = getInfoInBrakets s
        volume = init $ head info
        status = last info

getInfoInBrakets :: String -> [String]
getInfoInBrakets [] = []
getInfoInBrakets s = case takeWhile (/=']') $ dropWhile (/='[') s of
  ""     -> []
  (_:ss) -> ss:(getInfoInBrakets $ tail $ dropWhile (/='[') s)

formatVolume :: Int -> Int -> Logger -> Logger
formatVolume lo hi l = do
  ms <- l
  case ms of
    Just s -> return $ Just $ rDye s
    Nothing -> l
  where
    rDye = ( xmobarAddAction (Just 1) (xdotool "Super_L+Shift_L+F3")
           . xmobarAddAction (Just 3) (xdotool "Super_L+Shift_L+F2")
           . xmobarAddAction (Just 2) (xdotool "Super_L+Shift_L+F4")
           . (++"%")
           . dye
           . read)
    dye x = if x < 0
            then show x
            else if x <= lo
                 then xmobarColor "lightblue" "" (show x)
                 else if x >= hi
                      then xmobarColor "red" "" (show x)
                      else xmobarColor "green" "" (show x)

brightFile :: FilePath
brightFile = "/sys/class/backlight/intel_backlight/brightness"

setBright :: (Int -> Int) -> X ()
setBright f = liftIO $ withFile brightFile ReadWriteMode $ alterFile f

alterFile :: (Show a, Read a) => (a -> a) -> Handle -> IO ()
alterFile f h = do
  curr <- hGetLine h
  hPutStrLn h $ show $ f $ read curr

-- | Gets the currently focused window
focusedWindow :: X(Maybe Window)
focusedWindow = gets (W.peek . windowset)

-- | Sets currently focused window to float at screen corner
setToPlayerView :: X ()
setToPlayerView = do
  mw <- focusedWindow
  case mw of
    Just w -> w `floatTo` rr
    Nothing -> return ()
  where
    floatTo w rr = windows $ W.float w rr
    rr = W.RationalRect 0.65 0.65 0.33 0.33

hideString :: String -> String
hideString _ = ""

xdotool :: String -> String
xdotool = (++) "xdotool key \\-\\-clearmodifiers "

xmobarAddAction :: Maybe Int -> String -> (String -> String)
xmobarAddAction Nothing cmd =
  wrap
  ("<action=`" ++ cmd ++ "`>")
  "</action>"
xmobarAddAction (Just n) cmd =
  wrap
  ("<action=`" ++ cmd ++ "` button="++ (show n) ++">")
  "</action>"
