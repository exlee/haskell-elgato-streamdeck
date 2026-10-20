-- | Lists connected Stream Decks without opening them.
module Main (main) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import System.Hardware.StreamDeck

main :: IO ()
main =
  newHidApi >>= \case
    Left e -> hPutStrLn stderr ("Failed to create HidApi instance: " ++ e) >> exitFailure
    Right hid -> do
      devices <- listDevices hid
      printf "%d Stream Deck(s) found\n" (length devices)
      mapM_
        (\(k, serial) -> printf "%s %s 0x%04x\n" (kindToString k) serial (productId k))
        devices
