-- | Translation of examples/async.rs: prints input updates using the
-- concurrent wrapper.
module Main (main) where

import Control.Exception (throwIO, ErrorCall (..))
import Control.Monad (forM_, unless)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import Text.Printf (printf)

import System.Hardware.StreamDeck (Error, Update (..), errorToString, keyCount, newHidApi, updateToString)
import qualified System.Hardware.StreamDeck as SD
import qualified System.Hardware.StreamDeck.Async as A

ok :: Either Error a -> IO a
ok = either (throwIO . ErrorCall . errorToString) pure

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  newHidApi >>= \case
    Left e -> hPutStrLn stderr ("Failed to create HidApi instance: " ++ e)
    Right hid ->
      A.listDevices hid >>= mapM_ (\(k, serial) -> do
        device <- ok =<< A.connect hid k serial
        sn <- ok =<< A.serialNumber device
        fw <- ok =<< A.firmwareVersion device
        printf "Connected to '%s' with version '%s'\n" sn fw
        ok =<< A.setBrightness device 50
        ok =<< A.clearAllButtonImages device
        image <- ok =<< SD.loadFile "examples/no-place-like-localhost.jpg"
        forM_ [0 .. keyCount k - 1] $ \i -> ok =<< A.setButtonImage device i image
        ok =<< A.flush device
        reader <- A.getReader device
        let loop = do
              updates <- ok =<< A.readerRead reader 100
              mapM_ (putStrLn . updateToString) updates
              unless (ButtonUp (keyCount k - 1) `elem` updates) loop
        loop
        A.close device)
