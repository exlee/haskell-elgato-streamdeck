-- | Concurrent Stream Deck, translation of the Rust crate's asynchronous
-- module. GHC threads are preemptive and blocking hidapi calls are safe
-- foreign calls, so they do not stall other threads (the counterpart of
-- tokio's block_in_place). Device access is serialized by an STM lock.
module System.Hardware.StreamDeck.Async
  ( AsyncStreamDeck
  , asyncDevice
  , ofStreamDeck
  , refreshDeviceList
  , listDevices
  , connect
  , kind
  , manufacturer
  , product
  , serialNumber
  , firmwareVersion
  , reset
  , setBrightness
  , writeImage
  , writeLcd
  , writeLcdFill
  , clearButtonImage
  , clearAllButtonImages
  , setTouchpointColor
  , flush
  , close
  , convertImage
  , convertImageWithFormat
  , setButtonImage
  , readInput
  , Reader
  , readerDevice
  , getReader
  , readerRead
  ) where

import Prelude hiding (product)

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)

import qualified System.Hardware.StreamDeck as SD
import System.Hardware.StreamDeck.Device (DeviceState, StreamDeck)
import qualified System.Hardware.StreamDeck.Device as Device
import qualified System.Hardware.StreamDeck.HidApi as HidApi
import System.Hardware.StreamDeck.Image (Image)
import System.Hardware.StreamDeck.Images (ImageRect)
import qualified System.Hardware.StreamDeck.Images as Images
import System.Hardware.StreamDeck.Info (ImageFormat, Kind)
import System.Hardware.StreamDeck.Types

data AsyncStreamDeck = AsyncStreamDeck
  { asyncKind :: Kind
  , asyncLock :: TMVar ()
  , asyncDevice :: StreamDeck
  }

ofStreamDeck :: StreamDeck -> IO AsyncStreamDeck
ofStreamDeck device = AsyncStreamDeck (Device.kind device) <$> newTMVarIO () <*> pure device

refreshDeviceList :: HidApi.HidApi -> IO (Either String ())
refreshDeviceList = SD.refreshDeviceList

listDevices :: HidApi.HidApi -> IO [(Kind, String)]
listDevices = SD.listDevices

-- | Attempts to connect to the device.
connect :: HidApi.HidApi -> Kind -> String -> IO (Either Error AsyncStreamDeck)
connect hidapi k serial = Device.connect hidapi k serial >>= traverse ofStreamDeck

kind :: AsyncStreamDeck -> Kind
kind = asyncKind

-- | Runs a device operation while holding the device lock.
locked :: AsyncStreamDeck -> (StreamDeck -> IO a) -> IO a
locked t f =
  bracket_
    (atomically (takeTMVar (asyncLock t)))
    (atomically (putTMVar (asyncLock t) ()))
    (f (asyncDevice t))

manufacturer :: AsyncStreamDeck -> IO (Either Error String)
manufacturer t = locked t Device.manufacturer

product :: AsyncStreamDeck -> IO (Either Error String)
product t = locked t Device.product

serialNumber :: AsyncStreamDeck -> IO (Either Error String)
serialNumber t = locked t Device.serialNumber

firmwareVersion :: AsyncStreamDeck -> IO (Either Error String)
firmwareVersion t = locked t Device.firmwareVersion

reset :: AsyncStreamDeck -> IO (Either Error ())
reset t = locked t Device.reset

setBrightness :: AsyncStreamDeck -> Int -> IO (Either Error ())
setBrightness t percent = locked t (`Device.setBrightness` percent)

writeImage :: AsyncStreamDeck -> Int -> ByteString -> IO (Either Error ())
writeImage t key d = locked t (\dev -> Device.writeImage dev key d)

writeLcd :: AsyncStreamDeck -> Int -> Int -> ImageRect -> IO (Either Error ())
writeLcd t x y rect = locked t (\dev -> Device.writeLcd dev x y rect)

writeLcdFill :: AsyncStreamDeck -> ByteString -> IO (Either Error ())
writeLcdFill t d = locked t (`Device.writeLcdFill` d)

clearButtonImage :: AsyncStreamDeck -> Int -> IO (Either Error ())
clearButtonImage t key = locked t (`Device.clearButtonImage` key)

clearAllButtonImages :: AsyncStreamDeck -> IO (Either Error ())
clearAllButtonImages t = locked t Device.clearAllButtonImages

setTouchpointColor :: AsyncStreamDeck -> Int -> Int -> Int -> Int -> IO (Either Error ())
setTouchpointColor t point r g b = locked t (\dev -> Device.setTouchpointColor dev point r g b)

flush :: AsyncStreamDeck -> IO (Either Error ())
flush t = locked t Device.flush

close :: AsyncStreamDeck -> IO ()
close t = locked t Device.close

-- | Image conversion runs outside the device lock.
convertImage :: Kind -> Image -> IO (Either Error ByteString)
convertImage = Images.convertImage

convertImageWithFormat :: ImageFormat -> Image -> IO (Either Error ByteString)
convertImageWithFormat = Images.convertImageWithFormat

setButtonImage :: AsyncStreamDeck -> Int -> Image -> IO (Either Error ())
setButtonImage t key image = runExceptT $ do
  d <- ExceptT (convertImage (asyncKind t) image)
  ExceptT (writeImage t key d)

-- | Polls @pollRate@ times per second until input arrives. The lock is
-- released while sleeping.
readInput :: AsyncStreamDeck -> Double -> IO (Either Error Input)
readInput t pollRate =
  locked t (`Device.readInput` Nothing) >>= \case
    Right input | inputIsEmpty input -> do
      threadDelay (max 0 (round (1e6 / pollRate)))
      readInput t pollRate
    r -> pure r

-- | Reader that keeps device state and returns events instead of full
-- states.
data Reader = Reader
  { readerDevice :: AsyncStreamDeck
  , readerState :: TVar DeviceState
  }

getReader :: AsyncStreamDeck -> IO Reader
getReader t = Reader t <$> newTVarIO (Device.initialState (asyncKind t))

readerRead :: Reader -> Double -> IO (Either Error [Update])
readerRead r pollRate = runExceptT $ do
  input <- ExceptT (readInput (readerDevice r) pollRate)
  lift . atomically . stateTVar (readerState r) $ \s ->
    let (s', updates) = Device.applyInput (asyncKind (readerDevice r)) s input
     in (updates, s')
