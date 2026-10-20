-- | Interface for a Stream Deck device.
module System.Hardware.StreamDeck.Device
  ( -- * Transport
    Transport (..)
  , transportOfHidDevice
    -- * Device
  , StreamDeck
  , ofTransport
  , connect
  , close
  , kind
  , manufacturer
  , product
  , serialNumber
  , firmwareVersion
  , readInput
  , reset
  , setBrightness
  , imageDataReports
  , writeImageDataReports
  , keyReportParameters
  , sendImage
  , writeImage
  , writeLcd
  , writeLcdFill
  , clearButtonImage
  , clearAllButtonImages
  , setButtonImage
  , setTouchpointColor
  , flush
    -- * State tracking
  , DeviceState (..)
  , initialState
  , applyInput
  , Reader
  , readerDevice
  , getReader
  , readerRead
  ) where

import Prelude hiding (product)

import Control.Concurrent.MVar
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (catMaybes, fromJust, fromMaybe)

import qualified System.Hardware.StreamDeck.HidApi as HidApi
import System.Hardware.StreamDeck.Image (Image)
import System.Hardware.StreamDeck.Images (ImageRect (..))
import qualified System.Hardware.StreamDeck.Images as Images
import System.Hardware.StreamDeck.Info
import System.Hardware.StreamDeck.Types
import qualified System.Hardware.StreamDeck.Util as Util

-- | Device I/O used by 'StreamDeck'. 'transportOfHidDevice' backs it with
-- hidapi; tests substitute a fake.
data Transport = Transport
  { transportManufacturer :: IO (Either Error (Maybe String))
  , transportProduct :: IO (Either Error (Maybe String))
  , transportGetFeatureReport :: Int -> Int -> IO (Either Error ByteString)
  , transportSendFeatureReport :: ByteString -> IO (Either Error ())
  , transportRead :: Int -> Maybe Double -> IO (Either Error ByteString)
  , transportWrite :: ByteString -> IO (Either Error Int)
  , transportClose :: IO ()
  }

transportOfHidDevice :: HidApi.Device -> Transport
transportOfHidDevice dev =
  Transport
    { transportManufacturer = Util.hid <$> HidApi.getManufacturerString dev
    , transportProduct = Util.hid <$> HidApi.getProductString dev
    , transportGetFeatureReport = Util.getFeatureReport dev
    , transportSendFeatureReport = Util.sendFeatureReport dev
    , transportRead = Util.readData dev
    , transportWrite = Util.writeData dev
    , transportClose = HidApi.close dev
    }

data StreamDeck = StreamDeck
  { sdKind :: Kind
  , sdTransport :: Transport
  , -- | Images cached until 'flush', newest first.
    sdImageCache :: MVar [(Int, ByteString)]
  }

ofTransport :: Kind -> Transport -> IO StreamDeck
ofTransport k transport = StreamDeck k transport <$> newMVar []

-- | Attempts to connect to the device.
connect :: HidApi.HidApi -> Kind -> String -> IO (Either Error StreamDeck)
connect hidapi k serial =
  HidApi.openSerial hidapi (vendorId k) (productId k) serial >>= \case
    Left e -> pure (Left (HidError e))
    Right dev -> Right <$> ofTransport k (transportOfHidDevice dev)

close :: StreamDeck -> IO ()
close = transportClose . sdTransport

kind :: StreamDeck -> Kind
kind = sdKind

manufacturer :: StreamDeck -> IO (Either Error String)
manufacturer t = fmap (fromMaybe "Unknown") <$> transportManufacturer (sdTransport t)

product :: StreamDeck -> IO (Either Error String)
product t = fmap (fromMaybe "Unknown") <$> transportProduct (sdTransport t)

featureString :: StreamDeck -> Int -> Int -> Int -> IO (Either Error String)
featureString t reportId len offset = runExceptT $ do
  bytes <- ExceptT (transportGetFeatureReport (sdTransport t) reportId len)
  ExceptT (pure (Util.extractStr (B.drop offset bytes)))

isMiniFamily :: Kind -> Bool
isMiniFamily k = k `elem` [Original, Mini, MiniMk2, MiniDiscord, MiniMk2Module]

-- | Returns the serial number of the device.
serialNumber :: StreamDeck -> IO (Either Error String)
serialNumber t = fmap (filter (/= '\x01')) <$> case sdKind t of
  k | k `elem` [Original, Mini] -> featureString t 0x03 17 5
  k | k `elem` [MiniMk2, MiniDiscord, MiniMk2Module] -> featureString t 0x03 32 5
  _ -> featureString t 0x06 32 2

-- | Returns the firmware version of the device.
firmwareVersion :: StreamDeck -> IO (Either Error String)
firmwareVersion t = case sdKind t of
  k | k `elem` [Original, Mini, MiniMk2, MiniDiscord] -> featureString t 0x04 17 5
  MiniMk2Module -> featureString t 0xA1 17 5
  _ -> featureString t 0x05 32 6

-- | Reads all possible input. The timeout is in seconds; 'Nothing' does not
-- block.
readInput :: StreamDeck -> Maybe Double -> IO (Either Error Input)
readInput t timeout = runExceptT $ case k of
  _ | k `elem` [Plus, PlusXl] -> do
    let len = max (6 + keyCount k) (5 + encoderCount k)
    d <- ExceptT (transportRead (sdTransport t) len timeout)
    if Util.byte d 0 == 0
      then pure InputNoData
      else case Util.byte d 1 of
        0 -> pure (InputButtonStateChange (Util.readButtonStates k d))
        2 -> ExceptT (pure (Util.readLcdInput d))
        3 -> ExceptT (pure (Util.readEncoderInput k d))
        _ -> throwE BadData
  _ -> do
    let len =
          if isMiniFamily k
            then 1 + keyCount k
            else 4 + keyCount k + touchpointCount k
    d <- ExceptT (transportRead (sdTransport t) len timeout)
    pure $
      if Util.byte d 0 == 0
        then InputNoData
        else InputButtonStateChange (Util.readButtonStates k d)
  where
    k = sdKind t

bytesOfList :: [Int] -> ByteString
bytesOfList = B.pack . map (fromIntegral . (.&. 0xff))

padded :: [Int] -> Int -> ByteString
padded l n = bytesOfList l <> B.replicate n 0

-- | Resets the device.
reset :: StreamDeck -> IO (Either Error ())
reset t
  | isMiniFamily (sdKind t) = transportSendFeatureReport (sdTransport t) (padded [0x0B, 0x63] 15)
  | otherwise = transportSendFeatureReport (sdTransport t) (padded [0x03, 0x02] 30)

-- | Sets brightness, value range is 0 - 100.
setBrightness :: StreamDeck -> Int -> IO (Either Error ())
setBrightness t percent0
  | isMiniFamily (sdKind t) =
      transportSendFeatureReport (sdTransport t)
        (padded [0x05, 0x55, 0xaa, 0xd1, 0x01, percent] 11)
  | otherwise = transportSendFeatureReport (sdTransport t) (padded [0x03, 0x08, percent] 29)
  where
    percent = max 0 (min 100 percent0)

-- | Splits data into pages of @payloadLength@, each prefixed by
-- @header page thisLength last@ and zero-padded to @reportLength@.
imageDataReports :: ByteString -> Int -> Int -> (Int -> Int -> Bool -> [Int]) -> [ByteString]
imageDataReports d reportLength payloadLength0 header = go 0 d
  where
    payloadLength = max 1 payloadLength0
    go page remaining
      | B.null remaining = []
      | otherwise =
          let (chunk, rest) = B.splitAt payloadLength remaining
              thisLength = B.length chunk
              body = bytesOfList (header page thisLength (B.null rest)) <> chunk
              pad = max 0 (reportLength - B.length body)
           in (body <> B.replicate pad 0) : go (page + 1) rest

writeImageDataReports ::
  StreamDeck -> ByteString -> Int -> Int -> (Int -> Int -> Bool -> [Int]) -> IO (Either Error ())
writeImageDataReports t d reportLength payloadLength header =
  runExceptT $
    mapM_
      (\report -> ExceptT (transportWrite (sdTransport t) report))
      (imageDataReports d reportLength payloadLength header)

lo, hi :: Int -> Int
lo v = v .&. 0xff
hi v = (v `shiftR` 8) .&. 0xff

flag :: Bool -> Int
flag b = if b then 1 else 0

-- | Report length and payload length for key images.
keyReportParameters :: Kind -> Int -> (Int, Int)
keyReportParameters k dataLen = (reportLength, payloadLength)
  where
    reportLength = if k == Original then 8191 else 1024
    headerLength = if isMiniFamily k then 16 else 8
    payloadLength = if k == Original then dataLen `div` 2 else reportLength - headerLength

-- | Sends image data to a key immediately.
sendImage :: StreamDeck -> Int -> ByteString -> IO (Either Error ())
sendImage t key0 d
  | key0 < 0 || key0 >= keyCount k = pure (Left InvalidKeyIndex)
  | not (isVisual k) = pure (Left NoScreen)
  | otherwise =
      writeImageDataReports t d reportLength payloadLength $ \page thisLength lastPage ->
        case k of
          Original -> [0x02, 0x01, page + 1, 0, flag lastPage, key + 1] ++ zeros
          _ | k `elem` [Mini, MiniMk2, MiniDiscord, MiniMk2Module] ->
            [0x02, 0x01, page, 0, flag lastPage, key + 1] ++ zeros
          _ ->
            [0x02, 0x07, key, flag lastPage, lo thisLength, hi thisLength, lo page, hi page]
  where
    k = sdKind t
    key = if k == Original then Util.flipKeyIndex k key0 else key0
    (reportLength, payloadLength) = keyReportParameters k (B.length d)
    zeros = replicate 10 0

-- | Queues image data for a key; 'flush' sends it to the device.
writeImage :: StreamDeck -> Int -> ByteString -> IO (Either Error ())
writeImage t key d = Right <$> modifyMVar_ (sdImageCache t) (pure . ((key, d) :))

lcdRegionHeader :: Int -> Int -> Int -> Int -> Int -> Int -> Bool -> [Int]
lcdRegionHeader x y w h page thisLength lastPage =
  [ 0x02, 0x0c, lo x, hi x, lo y, hi y, lo w, hi w, lo h, hi h, flag lastPage
  , lo page, hi page, lo thisLength, hi thisLength, 0
  ]

-- | Writes image data to an LCD region. Only the Plus family supports
-- regions; use 'writeLcdFill' for the Neo.
writeLcd :: StreamDeck -> Int -> Int -> ImageRect -> IO (Either Error ())
writeLcd t x y rect
  | sdKind t `elem` [Plus, PlusXl] =
      writeImageDataReports t (rectData rect) 1024 (1024 - 16)
        (lcdRegionHeader x y (rectW rect) (rectH rect))
  | otherwise = pure (Left UnsupportedOperation)

-- | Fills the whole LCD with image data from
-- 'Images.convertImageWithFormat' and 'lcdImageFormat'.
writeLcdFill :: StreamDeck -> ByteString -> IO (Either Error ())
writeLcdFill t d = case sdKind t of
  Neo ->
    writeImageDataReports t d 1024 (1024 - 8) $ \page thisLength lastPage ->
      [0x02, 0x0b, 0, flag lastPage, lo thisLength, hi thisLength, lo page, hi page]
  k | k `elem` [Plus, PlusXl] ->
    let (w, h) = fromJust (lcdStripSize k)
     in writeImageDataReports t d 1024 (1024 - 16) (lcdRegionHeader 0 0 w h)
  _ -> pure (Left UnsupportedOperation)

-- | Sends a blank image to a key immediately.
clearButtonImage :: StreamDeck -> Int -> IO (Either Error ())
clearButtonImage t key = sendImage t key (blankImage (sdKind t))

clearAllButtonImages :: StreamDeck -> IO (Either Error ())
clearAllButtonImages t =
  runExceptT $ mapM_ (ExceptT . clearButtonImage t) [0 .. keyCount (sdKind t) - 1]

-- | Queues an image for a key; 'flush' sends it to the device.
setButtonImage :: StreamDeck -> Int -> Image -> IO (Either Error ())
setButtonImage t key image = runExceptT $ do
  d <- ExceptT (Images.convertImage (sdKind t) image)
  ExceptT (writeImage t key d)

setTouchpointColor :: StreamDeck -> Int -> Int -> Int -> Int -> IO (Either Error ())
setTouchpointColor t point red green blue
  | point < 0 || point >= touchpointCount k = pure (Left InvalidTouchPointIndex)
  | otherwise =
      transportSendFeatureReport (sdTransport t)
        (bytesOfList [0x03, 0x06, point + keyCount k, red, green, blue])
  where
    k = sdKind t

-- | Sends queued images to the device. The queue is kept when sending
-- fails.
flush :: StreamDeck -> IO (Either Error ())
flush t = modifyMVar (sdImageCache t) $ \cache -> do
  r <- runExceptT (mapM_ (\(key, d) -> ExceptT (sendImage t key d)) (reverse cache))
  pure (either (const cache) (const []) r, r)

-- | Device state that a reader compares against. Buttons include touch
-- points.
data DeviceState = DeviceState
  { stateButtons :: [Bool]
  , stateEncoders :: [Bool]
  }
  deriving (Eq, Show)

initialState :: Kind -> DeviceState
initialState k =
  DeviceState
    { stateButtons = replicate (keyCount k + touchpointCount k) False
    , stateEncoders = replicate (encoderCount k) False
    }

-- | Updates the state with the input and returns the changes.
applyInput :: Kind -> DeviceState -> Input -> (DeviceState, [Update])
applyInput k state = \case
  InputButtonStateChange buttons ->
    let updates = changed buttons (stateButtons state) $ \i down ->
          if i < kc
            then if down then ButtonDown i else ButtonUp i
            else if down then TouchPointDown (i - kc) else TouchPointUp (i - kc)
     in (state {stateButtons = buttons}, updates)
  InputEncoderStateChange encoders ->
    let updates = changed encoders (stateEncoders state) $ \i down ->
          if down then EncoderDown i else EncoderUp i
     in (state {stateEncoders = encoders}, updates)
  InputEncoderTwist twist ->
    (state, [EncoderTwist i c | (i, c) <- zip [0 ..] twist, c /= 0])
  InputTouchScreenPress x y -> (state, [TouchScreenPress x y])
  InputTouchScreenLongPress x y -> (state, [TouchScreenLongPress x y])
  InputTouchScreenSwipe s e -> (state, [TouchScreenSwipe s e])
  InputNoData -> (state, [])
  where
    kc = keyCount k
    changed theirs mine f =
      catMaybes
        [ if a /= b then Just (f i a) else Nothing
        | (i, a, b) <- zip3 [0 ..] theirs mine
        ]

-- | Reader that keeps device state and returns events instead of full
-- states.
data Reader = Reader
  { readerDevice :: StreamDeck
  , readerState :: MVar DeviceState
  }

getReader :: StreamDeck -> IO Reader
getReader t = Reader t <$> newMVar (initialState (sdKind t))

readerRead :: Reader -> Maybe Double -> IO (Either Error [Update])
readerRead r timeout = runExceptT $ do
  input <- ExceptT (readInput (readerDevice r) timeout)
  lift . modifyMVar (readerState r) $ \s ->
    pure (applyInput (sdKind (readerDevice r)) s input)
