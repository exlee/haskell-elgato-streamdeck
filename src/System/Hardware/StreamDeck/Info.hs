-- | Various information about Stream Deck devices.
module System.Hardware.StreamDeck.Info
  ( -- * Vendor and product IDs
    elgatoVendorId
  , pidStreamdeckOriginal
  , pidStreamdeckOriginalV2
  , pidStreamdeckMini
  , pidStreamdeckXl
  , pidStreamdeckXlV2
  , pidStreamdeckMk2
  , pidStreamdeckMk2ScissorKeys
  , pidStreamdeckMiniMk2
  , pidStreamdeckMiniDiscord
  , pidStreamdeckNeo
  , pidStreamdeckPedal
  , pidStreamdeckPlus
  , pidStreamdeckPlusXl
  , pidStreamdeckMiniMk2Module
  , pidStreamdeckMk2Module
  , pidStreamdeckXlV2Module
  , recognizedVendors
  , isVendorFamiliar
    -- * Image formats
  , ImageRotation (..)
  , ImageMirroring (..)
  , ImageMode (..)
  , ImageFormat (..)
  , defaultImageFormat
    -- * Device kinds
  , Kind (..)
  , allKinds
  , kindToString
  , kindOfString
  , productId
  , vendorId
  , kindOfVidPid
  , keyCount
  , rowCount
  , columnCount
  , encoderCount
  , touchpointCount
  , lcdStripSize
  , isVisual
  , keyLayout
  , keyImageFormat
  , lcdImageFormat
  , blankImage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (find)

import qualified System.Hardware.StreamDeck.Blank as Blank

elgatoVendorId :: Int
elgatoVendorId = 0x0fd9

pidStreamdeckOriginal, pidStreamdeckOriginalV2, pidStreamdeckMini,
  pidStreamdeckXl, pidStreamdeckXlV2, pidStreamdeckMk2,
  pidStreamdeckMk2ScissorKeys, pidStreamdeckMiniMk2, pidStreamdeckMiniDiscord,
  pidStreamdeckNeo, pidStreamdeckPedal, pidStreamdeckPlus, pidStreamdeckPlusXl,
  pidStreamdeckMiniMk2Module, pidStreamdeckMk2Module,
  pidStreamdeckXlV2Module :: Int
pidStreamdeckOriginal = 0x0060
pidStreamdeckOriginalV2 = 0x006d
pidStreamdeckMini = 0x0063
pidStreamdeckXl = 0x006c
pidStreamdeckXlV2 = 0x008f
pidStreamdeckMk2 = 0x0080
pidStreamdeckMk2ScissorKeys = 0x00a5
pidStreamdeckMiniMk2 = 0x0090
pidStreamdeckMiniDiscord = 0x00b3
pidStreamdeckNeo = 0x009a
pidStreamdeckPedal = 0x0086
pidStreamdeckPlus = 0x0084
pidStreamdeckPlusXl = 0x00c6
pidStreamdeckMiniMk2Module = 0x00b8
pidStreamdeckMk2Module = 0x00b9
pidStreamdeckXlV2Module = 0x00ba

recognizedVendors :: [Int]
recognizedVendors = [elgatoVendorId]

-- | True for vendor IDs recognized by the library.
isVendorFamiliar :: Int -> Bool
isVendorFamiliar vendor = vendor `elem` recognizedVendors

data ImageRotation = Rot0 | Rot90 | Rot180 | Rot270
  deriving (Eq, Show)

data ImageMirroring = NoMirror | MirrorX | MirrorY | MirrorBoth
  deriving (Eq, Show)

data ImageMode = NoImage | Bmp | Jpeg
  deriving (Eq, Show)

data ImageFormat = ImageFormat
  { formatMode :: ImageMode
  , formatSize :: (Int, Int)
  , formatRotation :: ImageRotation
  , formatMirror :: ImageMirroring
  }
  deriving (Eq, Show)

defaultImageFormat :: ImageFormat
defaultImageFormat = ImageFormat NoImage (0, 0) Rot0 NoMirror

-- | Device kinds. The derived 'Show' renders the same names as
-- 'kindToString'; the derived 'Ord' follows declaration order.
data Kind
  = Original
  | OriginalV2
  | Mini
  | Xl
  | XlV2
  | Mk2
  | Mk2Scissor
  | MiniMk2
  | MiniDiscord
  | Neo
  | Pedal
  | Plus
  | PlusXl
  | MiniMk2Module
  | Mk2Module
  | XlV2Module
  deriving (Eq, Ord, Show, Enum, Bounded)

allKinds :: [Kind]
allKinds = [minBound .. maxBound]

kindToString :: Kind -> String
kindToString = show

kindOfString :: String -> Maybe Kind
kindOfString s = find ((== s) . kindToString) allKinds

productId :: Kind -> Int
productId = \case
  Original -> pidStreamdeckOriginal
  OriginalV2 -> pidStreamdeckOriginalV2
  Mini -> pidStreamdeckMini
  Xl -> pidStreamdeckXl
  XlV2 -> pidStreamdeckXlV2
  Mk2 -> pidStreamdeckMk2
  Mk2Scissor -> pidStreamdeckMk2ScissorKeys
  MiniMk2 -> pidStreamdeckMiniMk2
  MiniDiscord -> pidStreamdeckMiniDiscord
  Neo -> pidStreamdeckNeo
  Pedal -> pidStreamdeckPedal
  Plus -> pidStreamdeckPlus
  PlusXl -> pidStreamdeckPlusXl
  MiniMk2Module -> pidStreamdeckMiniMk2Module
  Mk2Module -> pidStreamdeckMk2Module
  XlV2Module -> pidStreamdeckXlV2Module

vendorId :: Kind -> Int
vendorId _ = elgatoVendorId

-- | Creates a kind from vendor ID and product ID.
kindOfVidPid :: Int -> Int -> Maybe Kind
kindOfVidPid vid pid
  | vid /= elgatoVendorId = Nothing
  | otherwise = find ((== pid) . productId) allKinds

keyCount :: Kind -> Int
keyCount = \case
  k | k `elem` [Original, OriginalV2, Mk2, Mk2Scissor, Mk2Module] -> 15
  k | k `elem` [Mini, MiniMk2, MiniDiscord, MiniMk2Module] -> 6
  k | k `elem` [Xl, XlV2, XlV2Module] -> 32
  PlusXl -> 36
  Pedal -> 3
  _ -> 8 -- Neo, Plus

rowCount :: Kind -> Int
rowCount = \case
  k | k `elem` [Original, OriginalV2, Mk2, Mk2Scissor, Mk2Module] -> 3
  k | k `elem` [Mini, MiniMk2, MiniDiscord, MiniMk2Module] -> 2
  k | k `elem` [Xl, XlV2, XlV2Module, PlusXl] -> 4
  Pedal -> 1
  _ -> 2 -- Neo, Plus

columnCount :: Kind -> Int
columnCount = \case
  k | k `elem` [Original, OriginalV2, Mk2, Mk2Scissor, Mk2Module] -> 5
  k | k `elem` [Mini, MiniMk2, MiniDiscord, MiniMk2Module] -> 3
  k | k `elem` [Xl, XlV2, XlV2Module] -> 8
  PlusXl -> 9
  Pedal -> 3
  _ -> 4 -- Neo, Plus

encoderCount :: Kind -> Int
encoderCount = \case
  Plus -> 4
  PlusXl -> 6
  _ -> 0

touchpointCount :: Kind -> Int
touchpointCount = \case
  Neo -> 2
  _ -> 0

lcdStripSize :: Kind -> Maybe (Int, Int)
lcdStripSize = \case
  Plus -> Just (800, 100)
  PlusXl -> Just (100, 1200)
  Neo -> Just (248, 58)
  _ -> Nothing

isVisual :: Kind -> Bool
isVisual Pedal = False
isVisual _ = True

-- | Key layout as (rows, columns).
keyLayout :: Kind -> (Int, Int)
keyLayout k = (rowCount k, columnCount k)

keyImageFormat :: Kind -> ImageFormat
keyImageFormat = \case
  Original -> ImageFormat Bmp (72, 72) Rot0 MirrorBoth
  k | k `elem` [OriginalV2, Mk2, Mk2Scissor, Mk2Module] ->
    ImageFormat Jpeg (72, 72) Rot0 MirrorBoth
  k | k `elem` [Mini, MiniMk2, MiniDiscord, MiniMk2Module] ->
    ImageFormat Bmp (80, 80) Rot90 MirrorX
  k | k `elem` [Neo, Xl, XlV2, XlV2Module] ->
    ImageFormat Jpeg (96, 96) Rot0 MirrorBoth
  Plus -> ImageFormat Jpeg (120, 120) Rot0 NoMirror
  PlusXl -> ImageFormat Jpeg (120, 120) Rot270 NoMirror
  _ -> defaultImageFormat -- Pedal

-- | Image format of the LCD screen, used for filling the LCD.
lcdImageFormat :: Kind -> Maybe ImageFormat
lcdImageFormat = \case
  Neo -> Just (ImageFormat Jpeg (248, 58) Rot180 NoMirror)
  Plus -> Just (ImageFormat Jpeg (800, 100) Rot0 NoMirror)
  PlusXl -> Just (ImageFormat Jpeg (100, 1200) Rot270 NoMirror)
  _ -> Nothing

-- | Blank image data appropriate for the kind.
blankImage :: Kind -> ByteString
blankImage = \case
  k | k `elem` [Original, Mini, MiniMk2, MiniDiscord, MiniMk2Module] ->
    let (ws, hs) = formatSize (keyImageFormat k)
     in Blank.bmpHeader <> B.replicate (ws * hs * 3) 0
  k | k `elem` [OriginalV2, Mk2, Mk2Scissor, Mk2Module] -> Blank.jpeg72
  k | k `elem` [Neo, Xl, XlV2, XlV2Module] -> Blank.jpeg96
  k | k `elem` [Plus, PlusXl] -> Blank.jpeg120
  _ -> B.empty -- Pedal
