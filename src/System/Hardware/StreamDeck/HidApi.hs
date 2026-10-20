-- | Minimal binding to libhidapi, loaded at runtime with dlopen, covering
-- what the Rust hidapi crate provides to elgato-streamdeck.
module System.Hardware.StreamDeck.HidApi
  ( -- * Library handle
    HidApi
  , DeviceInfo (..)
  , create
  , refreshDevices
  , deviceList
  , enumerate
    -- * Devices
  , Device
  , openSerial
  , close
  , write
  , read
  , readTimeout
  , setBlockingMode
  , sendFeatureReport
  , getFeatureReport
  , getManufacturerString
  , getProductString
    -- * Wide strings
  , wstringOfPtr
  , withWString
  ) where

import Prelude hiding (read)

import Control.Monad (unless, void)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Char (chr, ord)
import Data.IORef
import Data.Word (Word16, Word32, Word8)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt (..), CSize (..), CUShort (..))
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (withArray0)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr
import Foreign.Storable (Storable (..))
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info

import System.Hardware.StreamDeck.DynLib (openFirst, symbol)

-- C function types, converted from dlsym results by "dynamic" imports.
-- Calls that may block are safe so other Haskell threads keep running.

type DevPtr = Ptr ()

type FnInit = IO CInt
type FnEnumerate = CUShort -> CUShort -> IO (Ptr ())
type FnFreeEnumeration = Ptr () -> IO ()
type FnOpen = CUShort -> CUShort -> Ptr () -> IO DevPtr
type FnClose = DevPtr -> IO ()
type FnError = DevPtr -> IO (Ptr ())
type FnBuffer = DevPtr -> Ptr Word8 -> CSize -> IO CInt
type FnReadTimeout = DevPtr -> Ptr Word8 -> CSize -> CInt -> IO CInt
type FnSetNonblocking = DevPtr -> CInt -> IO CInt
type FnString = DevPtr -> Ptr () -> CSize -> IO CInt

foreign import ccall unsafe "dynamic" mkInit :: FunPtr FnInit -> FnInit
foreign import ccall unsafe "dynamic" mkEnumerate :: FunPtr FnEnumerate -> FnEnumerate
foreign import ccall unsafe "dynamic"
  mkFreeEnumeration :: FunPtr FnFreeEnumeration -> FnFreeEnumeration
foreign import ccall unsafe "dynamic" mkOpen :: FunPtr FnOpen -> FnOpen
foreign import ccall unsafe "dynamic" mkClose :: FunPtr FnClose -> FnClose
foreign import ccall unsafe "dynamic" mkError :: FunPtr FnError -> FnError
foreign import ccall safe "dynamic" mkBuffer :: FunPtr FnBuffer -> FnBuffer
foreign import ccall safe "dynamic"
  mkReadTimeout :: FunPtr FnReadTimeout -> FnReadTimeout
foreign import ccall unsafe "dynamic"
  mkSetNonblocking :: FunPtr FnSetNonblocking -> FnSetNonblocking
foreign import ccall unsafe "dynamic" mkString :: FunPtr FnString -> FnString

data Fns = Fns
  { hidInit :: FnInit
  , hidEnumerate :: FnEnumerate
  , hidFreeEnumeration :: FnFreeEnumeration
  , hidOpen :: FnOpen
  , hidClose :: FnClose
  , hidError :: FnError
  , hidWrite :: FnBuffer
  , hidRead :: FnBuffer
  , hidReadTimeout :: FnReadTimeout
  , hidSetNonblocking :: FnSetNonblocking
  , hidSendFeatureReport :: FnBuffer
  , hidGetFeatureReport :: FnBuffer
  , hidGetManufacturerString :: FnString
  , hidGetProductString :: FnString
  }

-- | The library and its functions, loaded once on first use.
fnsOrError :: Either String Fns
fnsOrError = unsafePerformIO . runExceptT $ do
  dl <-
    ExceptT $
      openFirst
        "STREAMDECK_HIDAPI_LIB"
        [ "libhidapi.dylib"
        , "/opt/homebrew/lib/libhidapi.dylib"
        , "/usr/local/lib/libhidapi.dylib"
        , "libhidapi-hidraw.so.0"
        , "libhidapi-libusb.so.0"
        , "libhidapi.so.0"
        , "libhidapi.so"
        , "hidapi.dll"
        ]
  let fn :: (FunPtr a -> a) -> String -> ExceptT String IO a
      fn mk name = mk <$> ExceptT (symbol dl name)
  Fns
    <$> fn mkInit "hid_init"
    <*> fn mkEnumerate "hid_enumerate"
    <*> fn mkFreeEnumeration "hid_free_enumeration"
    <*> fn mkOpen "hid_open"
    <*> fn mkClose "hid_close"
    <*> fn mkError "hid_error"
    <*> fn mkBuffer "hid_write"
    <*> fn mkBuffer "hid_read"
    <*> fn mkReadTimeout "hid_read_timeout"
    <*> fn mkSetNonblocking "hid_set_nonblocking"
    <*> fn mkBuffer "hid_send_feature_report"
    <*> fn mkBuffer "hid_get_feature_report"
    <*> fn mkString "hid_get_manufacturer_string"
    <*> fn mkString "hid_get_product_string"
{-# NOINLINE fnsOrError #-}

-- | Functions for code that runs after 'create' succeeded.
fns :: Fns
fns = either error id fnsOrError

-- wchar_t is UTF-16 on Windows and UTF-32 elsewhere.

isWindows :: Bool
isWindows = System.Info.os == "mingw32"

wcharSize :: Int
wcharSize = if isWindows then 2 else 4

replacement :: Char
replacement = '\xFFFD'

toChar :: Int -> Char
toChar c
  | c < 0 || c > 0x10FFFF || (c >= 0xD800 && c < 0xE000) = replacement
  | otherwise = chr c

-- | Reads a NUL-terminated wchar_t string.
wstringOfPtr :: Ptr () -> IO String
wstringOfPtr p
  | isWindows = utf16 0
  | otherwise = utf32 0
  where
    at16 :: Int -> IO Int
    at16 i = fromIntegral <$> (peekElemOff (castPtr p) i :: IO Word16)
    utf16 i =
      at16 i >>= \case
        0 -> pure []
        hi | hi >= 0xD800 && hi < 0xDC00 -> do
          lo <- at16 (i + 1)
          if lo >= 0xDC00 && lo < 0xE000
            then (toChar (0x10000 + ((hi - 0xD800) `shiftL` 10) + (lo - 0xDC00)) :) <$> utf16 (i + 2)
            else (replacement :) <$> utf16 (i + 1)
        c -> (toChar c :) <$> utf16 (i + 1)
    utf32 i =
      (peekElemOff (castPtr p) i :: IO Word32) >>= \case
        0 -> pure []
        c -> (toChar (fromIntegral c) :) <$> utf32 (i + 1)

wstringOpt :: Ptr () -> IO (Maybe String)
wstringOpt p
  | p == nullPtr = pure Nothing
  | otherwise = Just <$> wstringOfPtr p

-- | Runs an action with a NUL-terminated wchar_t buffer holding the string.
withWString :: String -> (Ptr () -> IO a) -> IO a
withWString s act
  | isWindows = withArray0 (0 :: Word16) (concatMap units s) (act . castPtr)
  | otherwise = withArray0 (0 :: Word32) (map (fromIntegral . ord) s) (act . castPtr)
  where
    units ch
      | c < 0x10000 = [fromIntegral c]
      | otherwise =
          let c' = c - 0x10000
           in [fromIntegral (0xD800 + (c' `shiftR` 10)), fromIntegral (0xDC00 + (c' .&. 0x3FF))]
      where
        c = ord ch

-- Layout of struct hid_device_info, computed with C alignment rules:
-- path, vendor_id, product_id, serial_number, release_number,
-- manufacturer_string, product_string, usage_page, usage, interface_number,
-- next, bus_type.

-- | Offsets of the fields above, without bus_type.
data Offsets = Offsets Int Int Int Int Int Int Int Int Int Int Int

infoOffsets :: Offsets
infoOffsets = case scanl place (0, 0) fields of
  _ : offsets -> case map fst offsets of
    [a, b, c, d, e, f, g, h, i, j, k, _] -> Offsets a b c d e f g h i j k
    _ -> error "hid_device_info layout"
  [] -> error "hid_device_info layout"
  where
    ptrSize = sizeOf nullPtr
    u16Size = sizeOf (0 :: Word16)
    intSize = sizeOf (0 :: CInt)
    fields =
      [ptrSize, u16Size, u16Size, ptrSize, u16Size, ptrSize, ptrSize, u16Size, u16Size, intSize, ptrSize, intSize]
    -- Each field starts at the end of the previous one, rounded up to its
    -- own size (all fields here are scalars, so size equals alignment).
    place (start, prevSize) size =
      let end = start + prevSize
       in (((end + size - 1) `div` size) * size, size)

-- | Device as reported by hid_enumerate.
data DeviceInfo = DeviceInfo
  { diPath :: String
  , diVendorId :: Int
  , diProductId :: Int
  , diSerialNumber :: Maybe String
  , diReleaseNumber :: Int
  , diManufacturerString :: Maybe String
  , diProductString :: Maybe String
  , diUsagePage :: Int
  , diUsage :: Int
  , diInterfaceNumber :: Int
  }
  deriving (Eq, Show)

-- | Device list snapshot from the last enumeration.
newtype HidApi = HidApi (IORef [DeviceInfo])

-- | Opened device. A finalizer closes it when it becomes unreachable.
data Device = Device
  { devHandle :: ForeignPtr ()
  , devClosed :: IORef Bool
  }

errorMessage :: DevPtr -> IO String
errorMessage dev = do
  p <- hidError fns dev
  s <- if p == nullPtr then pure "" else wstringOfPtr p
  pure (if null s then "unknown hidapi error" else s)

enumerate :: IO [DeviceInfo]
enumerate = do
  root <- hidEnumerate fns 0 0
  if root == nullPtr
    then pure []
    else do
      devices <- walk root
      hidFreeEnumeration fns root
      pure devices
  where
    Offsets offPath offVendorId offProductId offSerialNumber offReleaseNumber
      offManufacturerString offProductString offUsagePage offUsage
      offInterfaceNumber offNext = infoOffsets
    u16 p o = fromIntegral <$> (peekByteOff p o :: IO Word16)
    walk p
      | p == nullPtr = pure []
      | otherwise = do
          pathP <- peekByteOff p offPath
          info <-
            DeviceInfo
              <$> (if pathP == nullPtr then pure "" else peekCString pathP)
              <*> u16 p offVendorId
              <*> u16 p offProductId
              <*> (peekByteOff p offSerialNumber >>= wstringOpt)
              <*> u16 p offReleaseNumber
              <*> (peekByteOff p offManufacturerString >>= wstringOpt)
              <*> (peekByteOff p offProductString >>= wstringOpt)
              <*> u16 p offUsagePage
              <*> u16 p offUsage
              <*> (fromIntegral <$> (peekByteOff p offInterfaceNumber :: IO CInt))
          next <- peekByteOff p offNext
          (info :) <$> walk next

-- | Initializes hidapi and takes a device list snapshot.
create :: IO (Either String HidApi)
create = case fnsOrError of
  Left e -> pure (Left e)
  Right f -> do
    rc <- hidInit f
    if rc /= 0
      then Left <$> errorMessage nullPtr
      else Right . HidApi <$> (enumerate >>= newIORef)

refreshDevices :: HidApi -> IO (Either String ())
refreshDevices (HidApi ref) = Right <$> (enumerate >>= writeIORef ref)

deviceList :: HidApi -> IO [DeviceInfo]
deviceList (HidApi ref) = readIORef ref

closeHandle :: IORef Bool -> DevPtr -> IO ()
closeHandle closedRef p = do
  wasClosed <- atomicModifyIORef' closedRef (True,)
  unless wasClosed (hidClose fns p)

close :: Device -> IO ()
close dev = withForeignPtr (devHandle dev) (closeHandle (devClosed dev))

openSerial :: HidApi -> Int -> Int -> String -> IO (Either String Device)
openSerial _ vid pid serial = do
  handle <- withWString serial (hidOpen fns (fromIntegral vid) (fromIntegral pid))
  if handle == nullPtr
    then Left <$> errorMessage nullPtr
    else do
      closedRef <- newIORef False
      fp <- FC.newForeignPtr handle (closeHandle closedRef handle)
      pure (Right (Device fp closedRef))

-- | Runs an hidapi call on an open device; negative results become errors.
withDevice :: Device -> (DevPtr -> IO CInt) -> IO (Either String Int)
withDevice dev f = withForeignPtr (devHandle dev) $ \p -> do
  closed <- readIORef (devClosed dev)
  if closed
    then pure (Left "device is closed")
    else do
      n <- f p
      if n < 0 then Left <$> errorMessage p else pure (Right (fromIntegral n))

write :: Device -> ByteString -> IO (Either String Int)
write dev payload =
  BU.unsafeUseAsCStringLen payload $ \(buf, len) ->
    withDevice dev $ \p -> hidWrite fns p (castPtr buf) (fromIntegral len)

-- | Reads into a buffer of @length@ bytes and returns the whole buffer.
readInto :: Device -> Int -> (DevPtr -> Ptr Word8 -> IO CInt) -> IO (Either String ByteString)
readInto dev len f = do
  fp <- BI.mallocByteString (max len 1)
  r <- withForeignPtr fp $ \buf -> do
    fillBytes buf 0 (max len 1)
    withDevice dev (`f` buf)
  pure (B.take len (BI.fromForeignPtr fp 0 (max len 1)) <$ r)

read :: Device -> Int -> IO (Either String ByteString)
read dev len = readInto dev len $ \p buf -> hidRead fns p buf (fromIntegral len)

readTimeout :: Device -> Int -> Int -> IO (Either String ByteString)
readTimeout dev len ms =
  readInto dev len $ \p buf -> hidReadTimeout fns p buf (fromIntegral len) (fromIntegral ms)

setBlockingMode :: Device -> Bool -> IO (Either String ())
setBlockingMode dev blocking =
  void <$> withDevice dev (\p -> hidSetNonblocking fns p (if blocking then 0 else 1))

sendFeatureReport :: Device -> ByteString -> IO (Either String ())
sendFeatureReport dev payload =
  BU.unsafeUseAsCStringLen payload $ \(buf, len) ->
    void <$> withDevice dev (\p -> hidSendFeatureReport fns p (castPtr buf) (fromIntegral len))

-- | The buffer holds the report ID in its first byte; returns the filled
-- buffer.
getFeatureReport :: Device -> ByteString -> IO (Either String ByteString)
getFeatureReport dev buf0 = do
  let len = B.length buf0
  fp <- BI.mallocByteString len
  r <- withForeignPtr fp $ \buf -> do
    BU.unsafeUseAsCString buf0 $ \src -> copyBytes buf (castPtr src) len
    withDevice dev $ \p -> hidGetFeatureReport fns p buf (fromIntegral len)
  pure (BI.fromForeignPtr fp 0 len <$ r)

getString :: (Fns -> FnString) -> Device -> IO (Either String (Maybe String))
getString f dev = do
  let maxlen = 256
  allocaBytes (maxlen * wcharSize) $ \buf -> do
    fillBytes buf 0 (maxlen * wcharSize)
    r <- withDevice dev $ \p -> f fns p buf (fromIntegral maxlen)
    case r of
      Left e -> pure (Left e)
      Right _ -> do
        s <- wstringOfPtr buf
        pure (Right (if null s then Nothing else Just s))

getManufacturerString :: Device -> IO (Either String (Maybe String))
getManufacturerString = getString hidGetManufacturerString

getProductString :: Device -> IO (Either String (Maybe String))
getProductString = getString hidGetProductString
