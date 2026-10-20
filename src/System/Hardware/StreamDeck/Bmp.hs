{-# LANGUAGE OverloadedStrings #-}

-- | Uncompressed 24-bit BMP encoding and decoding.
module System.Hardware.StreamDeck.Bmp
  ( rowStride
  , encode
  , decode
  ) where

import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)

import System.Hardware.StreamDeck.Image (Image (..))
import qualified System.Hardware.StreamDeck.Image as Image

rowStride :: Int -> Int
rowStride width = ((width * 3) + 3) .&. complement 3

encode :: Image -> ByteString
encode img = BL.toStrict . BB.toLazyByteString $ header <> foldMap row (reverse [0 .. h - 1])
  where
    w = imageWidth img
    h = imageHeight img
    stride = rowStride w
    imageSize = stride * h
    i32 = BB.int32LE . fromIntegral
    header =
      BB.string7 "BM" <> i32 (54 + imageSize) <> i32 0 <> i32 54 <> i32 40
        <> i32 w <> i32 h <> BB.word16LE 1 <> BB.word16LE 24 <> i32 0
        <> i32 imageSize <> i32 3780 <> i32 3780 <> i32 0 <> i32 0
    -- Rows are stored bottom-up as BGR.
    row y =
      foldMap
        (\x -> let (r, g, b) = Image.getPixel img x y in BB.word8 b <> BB.word8 g <> BB.word8 r)
        [0 .. w - 1]
        <> BB.byteString (B.replicate (stride - (w * 3)) 0)

decode :: ByteString -> Either String Image
decode s = do
  if len >= 54 && B.take 2 s == "BM" then Right () else Left "not a BMP file"
  let offset = i32 10
      width = i32 18
      height0 = i32 22
      bpp = u16 28
      compression = i32 30
  if (bpp == 24 || bpp == 32) && (compression == 0 || compression == 3) && width > 0 && height0 /= 0
    then Right ()
    else Left ("unsupported BMP (bpp " ++ show bpp ++ ", compression " ++ show compression ++ ")")
  let bottomUp = height0 > 0
      height = abs height0
      px = bpp `div` 8
      stride = ((width * px) + 3) .&. complement 3
  if offset + (stride * height) <= len then Right () else Left "truncated BMP"
  pure . Image.generate width height $ \x y ->
    let rowStart = offset + ((if bottomUp then height - 1 - y else y) * stride)
        o = rowStart + (x * px)
     in (B.index s (o + 2), B.index s (o + 1), B.index s o)
  where
    len = B.length s
    byte i = fromIntegral (B.index s i) :: Int
    u16 o = byte o .|. (byte (o + 1) `shiftL` 8)
    i32 :: Int -> Int
    i32 o =
      fromIntegral
        ( fromIntegral
            (byte o .|. (byte (o + 1) `shiftL` 8) .|. (byte (o + 2) `shiftL` 16) .|. (byte (o + 3) `shiftL` 24))
            :: Int32
        )
