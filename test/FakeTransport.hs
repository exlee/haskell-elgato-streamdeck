-- | Fake transport that records traffic and replays queued queuedReads.
module FakeTransport
  ( Fake (..)
  , newFake
  , device
  , bytes
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef

import System.Hardware.StreamDeck

data Fake = Fake
  { written :: IORef [ByteString]
  , featuresSent :: IORef [ByteString]
  , queuedReads :: IORef [ByteString]
  , featureReply :: IORef ByteString
  }

newFake :: IO Fake
newFake = Fake <$> newIORef [] <*> newIORef [] <*> newIORef [] <*> newIORef B.empty

-- | Device backed by the fake; queued queuedReads are zero-padded to the
-- requested length, and an empty queue queuedReads all zeros.
device :: Kind -> Fake -> IO StreamDeck
device k f =
  ofTransport k
    Transport
      { transportManufacturer = pure (Right Nothing)
      , transportProduct = pure (Right (Just "Stream Deck"))
      , transportGetFeatureReport = \_ _ -> Right <$> readIORef (featureReply f)
      , transportSendFeatureReport = \s -> Right <$> modifyIORef' (featuresSent f) (s :)
      , transportRead = \len _ ->
          atomicModifyIORef' (queuedReads f) $ \case
            r : rest -> (rest, Right (r <> B.replicate (max 0 (len - B.length r)) 0))
            [] -> ([], Right (B.replicate len 0))
      , transportWrite = \s -> do
          atomicModifyIORef' (written f) (\w -> (s : w, ()))
          pure (Right (B.length s))
      , transportClose = pure ()
      }

bytes :: [Int] -> ByteString
bytes = B.pack . map fromIntegral
