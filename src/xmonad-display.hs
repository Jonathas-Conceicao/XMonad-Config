-- Application for displaying information on screen
-- Author: Jonathas Conceição
-- https://github.com/Jonathas-Conceicao

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad.Extra (liftMaybe)
import Data.Maybe (fromJust)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (fetchName, queryTree)
import Sound.ALSA.Mixer
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.Mem.Weak (addFinalizer)

data Mode
  = Media
  | Brightness
  deriving (Read, Show)

main :: IO ()
main = do
  args <- getArgs
  let mode = read $ args !! 0
  drawBar mode
  exitWith ExitSuccess

drawBar :: Mode -> IO ()
drawBar mode = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      scr = defaultScreenOfDisplay dpy
  rootw <- rootWindow dpy dflt
  (_, _, ws) <- queryTree dpy rootw
  shouldLeave <- alreadyOpen dpy ws
  if shouldLeave
    then return ()
    else do
      win <- mkUnmanagedWindow dpy scr rootw 24 36 40 180
      setTextProperty dpy win "xmonad-display-media" wM_NAME
      mapWindow dpy win
      (gc, font, pixmap) <- drawInit dpy win
      loop dpy win gc font pixmap
  where
    loop dpy win gc font pixmap = do
      cur_info <- modeFunction mode
      drawInWin dpy win gc font pixmap (modeIcon mode) cur_info
      sync dpy False
      res <- race (waitChange cur_info) timeout
      case res of
        Left _ -> loop dpy win gc font pixmap
        Right _ -> return ()
    waitChange cur = do
      new <- modeFunction mode
      if cur /= new
        then return ()
        else threadDelay (300 * 1000) >> waitChange cur
    timeout = threadDelay (3 * 1000000)

alreadyOpen :: Display -> [Window] -> IO Bool
alreadyOpen _ [] = return True
alreadyOpen dpy xs = do
  mNames <- sequence $ fetchName dpy <$> xs
  return $ any (== Just "xmonad-display-media") mNames

modeFunction :: Mode -> IO Integer
modeFunction Brightness = brightnessInfo
modeFunction Media = mediaInfo

modeIcon :: Mode -> FilePath
modeIcon Brightness = "/home/jonathas/.xmonad/icons/bright.xbm"
modeIcon Media = "/home/jonathas/.xmonad/icons/sound_8.xbm"

mediaInfo :: IO Integer
mediaInfo = withMixer "default" $ \mixer -> do
  control <- getControlByName mixer "Master"
  vol <- getVolume control
  return $ fromJust vol
  where
    getVolume :: Maybe Control -> IO (Maybe Integer)
    getVolume Nothing = return Nothing
    getVolume (Just control) = do
      volumeStructure <- liftMaybe $ playback $ volume control
      (_, limit) <- getRange volumeStructure
      mvol <- getChannel FrontLeft $ value volumeStructure
      return $ (\v -> round $ toRational v / (toRational limit / 100)) <$> mvol

brightnessInfo :: IO Integer
brightnessInfo = do
  maxBright <- readFileValue "/sys/class/backlight/intel_backlight/max_brightness"
  curBright <- readFileValue "/sys/class/backlight/intel_backlight/brightness"
  return (round $ toRational curBright / (toRational maxBright / 100))
  where
    readFileValue f = readFile f >>= (\d -> return $ read $ d :: IO Integer)

drawImage :: Display -> Window -> GC -> FilePath -> IO ()
drawImage dpy win gc icon = do
  res <- readBitmapFile dpy win $ icon
  case res of
    Left s -> error $ "Failed to load image:" ++ s
    Right (w, h, p, _, _) -> do
      copyPlane dpy p win gc 0 0 w h 11 147 1
      freePixmap dpy p

drawInit :: Display -> Window -> IO (GC, FontStruct, Pixmap)
drawInit dpy win = do
  gc <- createGC dpy win
  fontStruc <- loadQueryFont dpy "-misc-*-*-*-*-*-8-*-*-*-*-*-*-*"
  p <- createPixmap dpy win 40 180 (defaultDepthOfScreen (defaultScreenOfDisplay dpy))

  addFinalizer gc (freeGC dpy gc)
  addFinalizer fontStruc (freeFont dpy fontStruc)
  addFinalizer p (freePixmap dpy p)

  return (gc, fontStruc, p)

drawInWin ::
  (Show a, Integral a) =>
  Display ->
  Window ->
  GC ->
  FontStruct ->
  Pixmap ->
  FilePath ->
  a ->
  IO ()
drawInWin dpy win gc font p icon val = do
  drawnBackground dpy p gc
  drawnFilled dpy p gc val
  drawnValue dpy p gc font val
  drawImage dpy p gc icon
  copyArea dpy p win gc 0 0 40 180 0 0

drawnBackground ::
  Display ->
  Drawable ->
  GC ->
  IO ()
drawnBackground dpy d gc = do
  bgColor <- initColor dpy "#44475a"
  setForeground dpy gc bgColor
  fillRectangle dpy d gc 0 0 40 180
  barColor <- initColor dpy "#f8f8f2"
  setForeground dpy gc barColor
  fillRectangle dpy d gc 10 5 20 135

drawnFilled ::
  (Integral a) =>
  Display ->
  Drawable ->
  GC ->
  a ->
  IO ()
drawnFilled dpy d gc v = do
  let val = toInteger v
      h = 130 - round (toRational val * 1.3)
  markerColor <- initColor dpy "#50fa7b"
  setForeground dpy gc markerColor
  fillRectangle dpy d gc 6 (fromInteger h) 28 14
  barColor <- initColor dpy "#bd93f9"
  setForeground dpy gc barColor
  fillRectangle dpy d gc 10 (fromInteger h + 14) 20 (130 - fromInteger h)

drawnValue ::
  (Show a) =>
  Display ->
  Drawable ->
  GC ->
  FontStruct ->
  a ->
  IO ()
drawnValue dpy d gc fontst val =
  do
    let str = (show val) ++ "%"
        w = textWidth fontst str
        (_, h, _, _) = textExtents fontst str
        valign = 180 - h
        offset = div (40 - w) 2
    fgcolor <- initColor dpy "#f8f8f2"
    bgcolor <- initColor dpy "#44475a"
    setForeground dpy gc fgcolor
    setBackground dpy gc bgcolor
    drawImageString dpy d gc offset valign str

mkUnmanagedWindow ::
  Display ->
  Screen ->
  Window ->
  Position ->
  Position ->
  Dimension ->
  Dimension ->
  IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
  win <- allocaSetWindowAttributes $
    \attributes -> do
      set_override_redirect attributes True
      createWindow
        dpy
        rw
        x
        y
        w
        h
        0
        (defaultDepthOfScreen scr)
        inputOutput
        visual
        cWOverrideRedirect
        attributes
  return win

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, _real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros
