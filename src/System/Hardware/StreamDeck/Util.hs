-- | Utility functions for working with Stream Deck devices.
module System.Hardware.StreamDeck.Util
  ( hid
  , getFeatureReport
  , sendFeatureReport
  , readData
  , writeData
  , extractStr
  , flipKeyIndex
  , byte
  , boolsFrom
  , readButtonStates
  , u16Le
  , readLcdInput
  , toI8
  , readEncoderInput
  ) where

import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified System.Hardware.StreamDeck.HidApi as HidApi
import System.Hardware.StreamDeck.Info (Kind (..))
import qualified System.Hardware.StreamDeck.Info as Info
import System.Hardware.StreamDeck.Types

hid :: Either String a -> Either Error a
hid = first HidError

-- | Performs get_feature_report; the result starts with the report ID byte.
getFeatureReport :: HidApi.Device -> Int -> Int -> IO (Either Error ByteString)
getFeatureReport dev reportId len =
  hid <$> HidApi.getFeatureReport dev (B.cons (fromIntegral reportId) (B.replicate len 0))

sendFeatureReport :: HidApi.Device -> ByteString -> IO (Either Error ())
sendFeatureReport dev payload = hid <$> HidApi.sendFeatureReport dev payload

-- | Reads @len@ bytes. Blocking mode is used when a timeout (seconds) is set.
readData :: HidApi.Device -> Int -> Maybe Double -> IO (Either Error ByteString)
readData dev len timeout = runExceptT $ do
  ExceptT $ hid <$> HidApi.setBlockingMode dev (isJust timeout)
  ExceptT $ hid <$> case timeout of
    Just t -> HidApi.readTimeout dev len (truncate (t * 1000))
    Nothing -> HidApi.read dev len

writeData :: HidApi.Device -> ByteString -> IO (Either Error Int)
writeData dev payload = hid <$> HidApi.write dev payload

-- | Extracts a string from bytes, removing NUL characters.
extractStr :: ByteString -> Either Error String
extractStr s = case TE.decodeUtf8' s of
  Right t -> Right (T.unpack (T.filter (/= '\0') t))
  Left _ -> Left Utf8Error

-- | Flips a key index horizontally, for the Original v1 Stream Deck.
flipKeyIndex :: Kind -> Int -> Int
flipKeyIndex kind key =
  let cols = Info.columnCount kind
      col = key `mod` cols
   in key - col + (cols - 1 - col)

byte :: ByteString -> Int -> Int
byte s i = fromIntegral (B.index s i)

boolsFrom :: ByteString -> Int -> [Bool]
boolsFrom s start = map (/= 0) (B.unpack (B.drop start s))

-- | Reads button states; empty when the report carries no data.
readButtonStates :: Kind -> ByteString -> [Bool]
readButtonStates kind states
  | byte states 0 == 0 = []
  | otherwise = case kind of
      Original ->
        [byte states (flipKeyIndex kind i + 1) /= 0 | i <- [0 .. Info.keyCount kind - 1]]
      k | k `elem` [Mini, MiniMk2, MiniDiscord, MiniMk2Module] -> boolsFrom states 1
      _ -> boolsFrom states 4

u16Le :: ByteString -> Int -> Int
u16Le s i = byte s i .|. (byte s (i + 1) `shiftL` 8)

-- | Reads LCD touch screen input.
readLcdInput :: ByteString -> Either Error Input
readLcdInput d =
  let start@(sx, sy) = (u16Le d 6, u16Le d 8)
   in case byte d 4 of
        1 -> Right (InputTouchScreenPress sx sy)
        2 -> Right (InputTouchScreenLongPress sx sy)
        3 -> Right (InputTouchScreenSwipe start (u16Le d 10, u16Le d 12))
        _ -> Left BadData

toI8 :: Int -> Int
toI8 b = if b >= 128 then b - 256 else b

-- | Reads encoder input.
readEncoderInput :: Kind -> ByteString -> Either Error Input
readEncoderInput kind d =
  let n = Info.encoderCount kind
      at i = byte d (5 + i)
   in case byte d 4 of
        0 -> Right (InputEncoderStateChange [at i /= 0 | i <- [0 .. n - 1]])
        1 -> Right (InputEncoderTwist [toI8 (at i) | i <- [0 .. n - 1]])
        _ -> Left BadData
