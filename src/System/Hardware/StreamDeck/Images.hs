{-# LANGUAGE OverloadedStrings #-}

-- | Image processing for Stream Deck keys and LCD screens.
module System.Hardware.StreamDeck.Images
  ( convertImageWithFormat
  , convertImage
  , loadFromMemory
  , loadFile
  , ImageRect (..)
  , imageRectOfImage
  ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified System.Hardware.StreamDeck.Bmp as Bmp
import System.Hardware.StreamDeck.Image (Image (..))
import qualified System.Hardware.StreamDeck.Image as Image
import System.Hardware.StreamDeck.Info
import qualified System.Hardware.StreamDeck.Jpeg as Jpeg
import System.Hardware.StreamDeck.Types (Error (..))

imageError :: Either String a -> Either Error a
imageError = first ImageError

-- | Converts an image into device image data for the format.
convertImageWithFormat :: ImageFormat -> Image -> IO (Either Error ByteString)
convertImageWithFormat format image0 =
  case formatMode format of
    NoImage -> pure (Right "")
    Bmp -> pure (Right (Bmp.encode image))
    Jpeg -> imageError <$> Jpeg.encodeWithQuality 90 image
  where
    (ws, hs) = formatSize format
    rotated = case formatRotation format of
      Rot0 -> image0
      Rot90 -> Image.rotate90 image0
      Rot180 -> Image.rotate180 image0
      Rot270 -> Image.rotate270 image0
    resized =
      if Image.dimensions rotated == (ws, hs) then rotated else Image.resizeExact rotated ws hs
    image = case formatMirror format of
      NoMirror -> resized
      MirrorX -> Image.fliph resized
      MirrorY -> Image.flipv resized
      MirrorBoth -> Image.flipv (Image.fliph resized)

-- | Converts an image into key image data for the kind.
convertImage :: Kind -> Image -> IO (Either Error ByteString)
convertImage kind = convertImageWithFormat (keyImageFormat kind)

-- | Decodes JPEG or BMP data.
loadFromMemory :: ByteString -> IO (Either Error Image)
loadFromMemory s
  | "BM" `B.isPrefixOf` s = pure (imageError (Bmp.decode s))
  | "\xff\xd8" `B.isPrefixOf` s = imageError <$> Jpeg.decode s
  | otherwise = pure (Left (ImageError "unrecognized image format"))

loadFile :: FilePath -> IO (Either Error Image)
loadFile path =
  try (B.readFile path) >>= \case
    Right s -> loadFromMemory s
    Left (e :: IOException) -> pure (Left (ImageError (show e)))

-- | Rect sent to an LCD screen region: JPEG data with its size.
data ImageRect = ImageRect
  { rectW :: Int
  , rectH :: Int
  , rectData :: ByteString
  }
  deriving (Eq, Show)

imageRectOfImage :: Image -> IO (Either Error ImageRect)
imageRectOfImage image =
  fmap (ImageRect (imageWidth image) (imageHeight image)) . imageError
    <$> Jpeg.encodeWithQuality 90 image
