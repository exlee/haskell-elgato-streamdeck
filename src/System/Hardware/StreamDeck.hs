-- | Elgato Stream Deck library: Haskell translation of the elgato-streamdeck
-- Rust crate, talking to libhidapi loaded at runtime.
module System.Hardware.StreamDeck
  ( module System.Hardware.StreamDeck.Types
  , module System.Hardware.StreamDeck.Info
  , module System.Hardware.StreamDeck.Device
  , Image (..)
  , Images.ImageRect (..)
  , Images.convertImage
  , Images.convertImageWithFormat
  , Images.loadFromMemory
  , Images.loadFile
  , Images.imageRectOfImage
  , HidApi.HidApi
  , newHidApi
  , refreshDeviceList
  , listDevices
  ) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import System.Hardware.StreamDeck.Device
import qualified System.Hardware.StreamDeck.HidApi as HidApi
import System.Hardware.StreamDeck.Image (Image (..))
import qualified System.Hardware.StreamDeck.Images as Images
import System.Hardware.StreamDeck.Info
import System.Hardware.StreamDeck.Types

-- | Creates a hidapi instance.
newHidApi :: IO (Either String HidApi.HidApi)
newHidApi = HidApi.create

-- | Re-enumerates devices.
refreshDeviceList :: HidApi.HidApi -> IO (Either String ())
refreshDeviceList = HidApi.refreshDevices

-- | Devices as (kind, serial) from the last enumeration, sorted and without
-- duplicates. Use 'refreshDeviceList' to refresh.
listDevices :: HidApi.HidApi -> IO [(Kind, String)]
listDevices api = Set.toAscList . Set.fromList . mapMaybe recognize <$> HidApi.deviceList api
  where
    recognize d
      | not (isVendorFamiliar (HidApi.diVendorId d)) = Nothing
      | otherwise =
          (,)
            <$> kindOfVidPid (HidApi.diVendorId d) (HidApi.diProductId d)
            <*> HidApi.diSerialNumber d
