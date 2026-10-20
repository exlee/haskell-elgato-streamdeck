-- | Translation of examples/simple.rs. Run from the package root:
-- cabal run simple
module Main (main) where

import Control.Exception (throwIO, ErrorCall (..))
import Control.Monad (forM_, unless)
import Data.Foldable (for_)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import Text.Printf (printf)

import System.Hardware.StreamDeck

ok :: Either Error a -> IO a
ok = either (throwIO . ErrorCall . errorToString) pure

run :: HidApi -> (Kind, String) -> IO ()
run hid (k, serial) = do
  printf "%s %s %d\n" (kindToString k) serial (productId k)
  device <- ok =<< connect hid k serial
  sn <- ok =<< serialNumber device
  fw <- ok =<< firmwareVersion device
  printf "Connected to '%s' with version '%s'\n" sn fw
  ok =<< setBrightness device 50
  ok =<< clearAllButtonImages device
  image <- ok =<< loadFile "examples/no-place-like-localhost.jpg"
  printf "Key count: %d\n" (keyCount k)
  forM_ [0 .. keyCount k - 1] $ \i -> ok =<< setButtonImage device i image
  printf "Touch point count: %d\n" (touchpointCount k)
  forM_ [0 .. touchpointCount k - 1] $ \i -> ok =<< setTouchpointColor device i 255 255 255
  for_ (lcdImageFormat k) $ \format -> do
    d <- ok =<< convertImageWithFormat format image
    _ <- writeLcdFill device d
    pure ()
  small <- case (lcdStripSize k, lcdImageFormat k) of
    (Just (w, h), Just format) -> do
      let m = min w h
      d <- ok =<< convertImageWithFormat format {formatSize = (m, m)} image
      Just <$> (ok =<< imageRectOfImage =<< ok =<< loadFromMemory d)
    _ -> pure Nothing
  ok =<< flush device
  reader <- getReader device
  let report = \case
        ButtonDown b -> printf "Button %d down\n" b
        ButtonUp b -> printf "Button %d up\n" b
        EncoderTwist d t -> printf "Dial %d twisted by %d\n" d t
        EncoderDown d -> printf "Dial %d down\n" d
        EncoderUp d -> printf "Dial %d up\n" d
        TouchPointDown p -> printf "Touch point %d down\n" p
        TouchPointUp p -> printf "Touch point %d up\n" p
        TouchScreenPress x y -> do
          printf "Touch Screen press at %d, %d\n" x y
          for_ small $ \s -> ok =<< writeLcd device x y s
        TouchScreenLongPress x y -> printf "Touch Screen long press at %d, %d\n" x y
        TouchScreenSwipe (sx, sy) (ex, ey) ->
          printf "Touch Screen swipe from %d, %d to %d, %d\n" sx sy ex ey
      loop =
        readerRead reader (Just 100) >>= \case
          Left _ -> pure ()
          Right updates -> do
            mapM_ report updates
            unless (ButtonUp (keyCount k - 1) `elem` updates) loop
  loop
  close device

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  newHidApi >>= \case
    Right hid -> listDevices hid >>= mapM_ (run hid)
    Left e -> hPutStrLn stderr ("Failed to create HidApi instance: " ++ e)
