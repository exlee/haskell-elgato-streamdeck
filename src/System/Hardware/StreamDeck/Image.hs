-- | RGB8 raster with the operations elgato-streamdeck takes from the Rust
-- image crate: rotation, mirroring and triangle-filter resizing.
module System.Hardware.StreamDeck.Image
  ( Image (..)
  , Rgb
  , create
  , generate
  , ofRgb
  , toRgb
  , dimensions
  , getPixel
  , setPixel
  , fill
  , remap
  , rotate90
  , rotate180
  , rotate270
  , fliph
  , flipv
  , weights
  , resizeExact
  ) where

import Control.Monad (forM_)
import Data.Array.ST (newArray, runSTUArray, writeArray)
import Data.Array.Unboxed (UArray, bounds, elems, listArray, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Foreign.Storable (pokeByteOff)

-- | Pixels are row-major RGB, three bytes per pixel.
data Image = Image
  { imageWidth :: !Int
  , imageHeight :: !Int
  , imageData :: !ByteString
  }
  deriving (Eq, Show)

type Rgb = (Word8, Word8, Word8)

-- | Black image.
create :: Int -> Int -> Image
create w h
  | w < 0 || h < 0 = error "Image.create"
  | otherwise = Image w h (B.replicate (w * h * 3) 0)

-- | Builds an image whose pixel (x, y) is @f x y@.
generate :: Int -> Int -> (Int -> Int -> Rgb) -> Image
generate w h f
  | w < 0 || h < 0 = error "Image.generate"
  | otherwise = Image w h . BI.unsafeCreate (w * h * 3) $ \p ->
      forM_ [0 .. h - 1] $ \y -> forM_ [0 .. w - 1] $ \x -> do
        let (r, g, b) = f x y
            i = ((y * w) + x) * 3
        pokeByteOff p i r
        pokeByteOff p (i + 1) g
        pokeByteOff p (i + 2) b

ofRgb :: Int -> Int -> ByteString -> Image
ofRgb w h bytes
  | B.length bytes /= w * h * 3 = error "Image.ofRgb"
  | otherwise = Image w h bytes

toRgb :: Image -> ByteString
toRgb = imageData

dimensions :: Image -> (Int, Int)
dimensions t = (imageWidth t, imageHeight t)

getPixel :: Image -> Int -> Int -> Rgb
getPixel t x y =
  let i = ((y * imageWidth t) + x) * 3
      d = imageData t
   in (B.index d i, B.index d (i + 1), B.index d (i + 2))

-- | Copy of the image with one pixel replaced.
setPixel :: Image -> Int -> Int -> Rgb -> Image
setPixel t x y (r, g, b) =
  let i = ((y * imageWidth t) + x) * 3
      (before, rest) = B.splitAt i (imageData t)
   in t {imageData = before <> B.pack [r, g, b] <> B.drop 3 rest}

-- | Image of the same size in one color.
fill :: Image -> Rgb -> Image
fill t rgb = generate (imageWidth t) (imageHeight t) (\_ _ -> rgb)

-- | Builds a w x h image whose pixel (x, y) is the source pixel @f x y@.
remap :: Image -> Int -> Int -> (Int -> Int -> (Int, Int)) -> Image
remap t w h f = Image w h . BI.unsafeCreate (w * h * 3) $ \p ->
  forM_ [0 .. h - 1] $ \y -> forM_ [0 .. w - 1] $ \x -> do
    let (sx, sy) = f x y
        si = ((sy * imageWidth t) + sx) * 3
        di = ((y * w) + x) * 3
    forM_ [0 .. 2] $ \c ->
      pokeByteOff p (di + c) (BU.unsafeIndex (imageData t) (si + c))

-- | Rotates 90 degrees clockwise.
rotate90 :: Image -> Image
rotate90 t = remap t (imageHeight t) (imageWidth t) (\x y -> (y, imageHeight t - 1 - x))

rotate180 :: Image -> Image
rotate180 t =
  remap t (imageWidth t) (imageHeight t) (\x y -> (imageWidth t - 1 - x, imageHeight t - 1 - y))

-- | Rotates 90 degrees counter-clockwise.
rotate270 :: Image -> Image
rotate270 t = remap t (imageHeight t) (imageWidth t) (\x y -> (imageWidth t - 1 - y, x))

fliph :: Image -> Image
fliph t = remap t (imageWidth t) (imageHeight t) (\x y -> (imageWidth t - 1 - x, y))

flipv :: Image -> Image
flipv t = remap t (imageWidth t) (imageHeight t) (\x y -> (x, imageHeight t - 1 - y))

triangle :: Double -> Double
triangle x = let a = abs x in if a < 1 then 1 - a else 0

-- | Per output coordinate: first source index and normalized weights.
-- Matches the sampling of image::imageops::resize.
weights :: Int -> Int -> [(Int, UArray Int Double)]
weights src dst = map entry [0 .. dst - 1]
  where
    ratio = fromIntegral src / fromIntegral dst :: Double
    sratio = max ratio 1
    support = sratio
    entry o =
      let center = (fromIntegral o + 0.5) * ratio
          left = max 0 (min (floor (center - support)) (src - 1))
          right = max (left + 1) (min (ceiling (center + support)) src)
          center' = center - 0.5
          ws = [triangle ((fromIntegral i - center') / sratio) | i <- [left .. right - 1]]
          total = sum ws
          normalized = if total > 0 then map (/ total) ws else ws
       in (left, listArray (0, length ws - 1) normalized)

-- | Rounds half away from zero and clamps to a byte.
clampByte :: Double -> Word8
clampByte v
  | r < 0 = 0
  | r > 255 = 255
  | otherwise = fromIntegral (r :: Int)
  where
    r = if v < 0 then ceiling (v - 0.5) else floor (v + 0.5)

-- | Resizes to exactly w x h using a triangle (bilinear) filter.
resizeExact :: Image -> Int -> Int -> Image
resizeExact t w h
  | w == tw && h == th = t
  | tw == 0 || th == 0 = create w h
  | otherwise = Image w h . BI.unsafeCreate (w * h * 3) $ \p ->
      -- Horizontal pass over the vertically filtered float buffer.
      forM_ (zip [0 ..] hw) $ \(x, (left, ws)) ->
        forM_ [0 .. h - 1] $ \y -> forM_ [0 .. 2] $ \c -> do
          let acc =
                sum
                  [ wgt * mid ! ((((y * tw) + left + i) * 3) + c)
                  | (i, wgt) <- zip [0 ..] (elems ws)
                  ]
          pokeByteOff p ((((y * w) + x) * 3) + c) (clampByte acc)
  where
    tw = imageWidth t
    th = imageHeight t
    src = imageData t
    vw = weights th h
    hw = weights tw w
    -- Vertical pass into a float buffer.
    mid :: UArray Int Double
    mid = runSTUArray $ do
      arr <- newArray (0, tw * h * 3 - 1) 0
      forM_ (zip [0 ..] vw) $ \(y, (left, ws)) ->
        forM_ [0 .. tw - 1] $ \x -> forM_ [0 .. 2] $ \c -> do
          let (_, hi) = bounds ws
              acc =
                sum
                  [ (ws ! i) * fromIntegral (BU.unsafeIndex src ((((left + i) * tw) + x) * 3 + c))
                  | i <- [0 .. hi]
                  ]
          writeArray arr (((y * tw) + x) * 3 + c) acc
      pure arr
