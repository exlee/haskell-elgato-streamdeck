-- | Tests for the concurrent device wrapper.
module TestAsync (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM, forM_)
import qualified Data.ByteString as B
import Data.IORef
import Data.List (nub, sort)

import Assert
import FakeTransport
import System.Hardware.StreamDeck
import qualified System.Hardware.StreamDeck.Async as A

tests :: Suite -> IO ()
tests suite = do
  test suite "async reader polls until input arrives" $ do
    f <- newFake
    -- Two empty polls precede the key press.
    writeIORef (queuedReads f) [B.empty, B.empty, bytes [1, 0, 0, 0, 0, 0, 1]]
    d <- A.ofStreamDeck =<< device Mk2 f
    r <- A.getReader d
    updates <- ok =<< A.readerRead r 1000
    check "updates" (updates == [ButtonDown 2])
    remaining <- readIORef (queuedReads f)
    check "all queuedReads consumed" (null remaining)

  test suite "async threads queue and flush images" $ do
    f <- newFake
    d <- A.ofStreamDeck =<< device Mk2 f
    let blank = blankImage Mk2
    -- Concurrent threads share the device without interleaving reports.
    dones <- forM [0 .. 3] $ \key -> do
      done <- newEmptyMVar
      _ <- forkIO (A.writeImage d key blank >>= putMVar done)
      pure done
    forM_ dones (\done -> takeMVar done >>= ok)
    ok =<< A.flush d
    let pages = length (imageDataReports blank 1024 1016 (\_ _ _ -> []))
    sent <- readIORef (written f)
    check "report count" (length sent == 4 * pages)
    let keys = map (`B.index` 2) sent
    check "keys" (sort (nub keys) == [0, 1, 2, 3])
