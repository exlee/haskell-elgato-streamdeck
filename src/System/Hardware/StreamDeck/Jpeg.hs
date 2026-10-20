-- | JPEG encoding and decoding through libturbojpeg, loaded at runtime with
-- dlopen.
module System.Hardware.StreamDeck.Jpeg
  ( encode
  , encodeWithQuality
  , decode
  ) where

import Control.Exception (bracket)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..), CULong (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

import System.Hardware.StreamDeck.DynLib (openFirst, symbol)
import System.Hardware.StreamDeck.Image (Image (..))
import qualified System.Hardware.StreamDeck.Image as Image

type Handle = Ptr ()

type FnInit = IO Handle
type FnDestroy = Handle -> IO CInt
type FnFree = Ptr Word8 -> IO ()
type FnErrorStr = Handle -> IO CString
type FnCompress2 =
  Handle -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> Ptr (Ptr Word8)
    -> Ptr CULong -> CInt -> CInt -> CInt -> IO CInt
type FnDecompressHeader3 =
  Handle -> Ptr Word8 -> CULong -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
type FnDecompress2 =
  Handle -> Ptr Word8 -> CULong -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "dynamic" mkInit :: FunPtr FnInit -> FnInit
foreign import ccall unsafe "dynamic" mkDestroy :: FunPtr FnDestroy -> FnDestroy
foreign import ccall unsafe "dynamic" mkFree :: FunPtr FnFree -> FnFree
foreign import ccall unsafe "dynamic" mkErrorStr :: FunPtr FnErrorStr -> FnErrorStr
foreign import ccall safe "dynamic" mkCompress2 :: FunPtr FnCompress2 -> FnCompress2
foreign import ccall unsafe "dynamic"
  mkDecompressHeader3 :: FunPtr FnDecompressHeader3 -> FnDecompressHeader3
foreign import ccall safe "dynamic" mkDecompress2 :: FunPtr FnDecompress2 -> FnDecompress2

data Fns = Fns
  { tjInitCompress :: FnInit
  , tjInitDecompress :: FnInit
  , tjDestroy :: FnDestroy
  , tjFree :: FnFree
  , tjGetErrorStr2 :: FnErrorStr
  , tjCompress2 :: FnCompress2
  , tjDecompressHeader3 :: FnDecompressHeader3
  , tjDecompress2 :: FnDecompress2
  }

-- | The library and its functions, loaded once on first use.
fnsOrError :: Either String Fns
fnsOrError = unsafePerformIO . runExceptT $ do
  dl <-
    ExceptT $
      openFirst
        "STREAMDECK_TURBOJPEG_LIB"
        [ "libturbojpeg.dylib"
        , "/opt/homebrew/lib/libturbojpeg.dylib"
        , "/opt/homebrew/opt/jpeg-turbo/lib/libturbojpeg.dylib"
        , "/usr/local/lib/libturbojpeg.dylib"
        , "libturbojpeg.so.0"
        , "libturbojpeg.so"
        , "turbojpeg.dll"
        ]
  let fn :: (FunPtr a -> a) -> String -> ExceptT String IO a
      fn mk name = mk <$> ExceptT (symbol dl name)
  Fns
    <$> fn mkInit "tjInitCompress"
    <*> fn mkInit "tjInitDecompress"
    <*> fn mkDestroy "tjDestroy"
    <*> fn mkFree "tjFree"
    <*> fn mkErrorStr "tjGetErrorStr2"
    <*> fn mkCompress2 "tjCompress2"
    <*> fn mkDecompressHeader3 "tjDecompressHeader3"
    <*> fn mkDecompress2 "tjDecompress2"
{-# NOINLINE fnsOrError #-}

tjPfRgb, tjSamp444 :: CInt
tjPfRgb = 0
tjSamp444 = 0

withHandle :: (Fns -> FnInit) -> (Fns -> Handle -> IO (Either String a)) -> IO (Either String a)
withHandle initFn f = case fnsOrError of
  Left e -> pure (Left e)
  Right fns -> do
    h <- initFn fns
    if h == nullPtr
      then pure (Left "turbojpeg: cannot create handle")
      else bracket (pure h) (tjDestroy fns) (f fns)

errorString :: Fns -> Handle -> IO (Either String a)
errorString fns h = Left . ("turbojpeg: " ++) <$> (tjGetErrorStr2 fns h >>= peekCString)

-- | Encodes with 4:4:4 chroma sampling at quality 90.
encode :: Image -> IO (Either String ByteString)
encode = encodeWithQuality 90

-- | Encodes with 4:4:4 chroma sampling at the given quality (1-100).
encodeWithQuality :: Int -> Image -> IO (Either String ByteString)
encodeWithQuality quality img = withHandle tjInitCompress $ \fns h ->
  BU.unsafeUseAsCString (Image.toRgb img) $ \src ->
    alloca $ \out -> alloca $ \outSize -> do
      poke out nullPtr
      poke outSize 0
      rc <-
        tjCompress2 fns h (castPtr src) (fromIntegral (imageWidth img)) 0
          (fromIntegral (imageHeight img)) tjPfRgb out outSize tjSamp444
          (fromIntegral quality) 0
      buf <- peek out
      result <-
        if rc /= 0
          then errorString fns h
          else do
            n <- fromIntegral <$> peek outSize
            Right <$> B.packCStringLen (castPtr buf, n)
      if buf /= nullPtr then tjFree fns buf else pure ()
      pure result

decode :: ByteString -> IO (Either String Image)
decode s = withHandle tjInitDecompress $ \fns h ->
  BU.unsafeUseAsCStringLen s $ \(src0, len0) -> do
    let src = castPtr src0
        len = fromIntegral len0
    alloca $ \wP -> alloca $ \hP -> alloca $ \ssP -> alloca $ \csP -> do
      rc <- tjDecompressHeader3 fns h src len wP hP ssP csP
      if rc /= 0
        then errorString fns h
        else do
          w <- fromIntegral <$> peek wP
          ht <- fromIntegral <$> peek hP
          fp <- BI.mallocByteString (w * ht * 3)
          rc2 <- withForeignPtr fp $ \dst ->
            tjDecompress2 fns h src len dst (fromIntegral w) 0 (fromIntegral ht) tjPfRgb 0
          if rc2 /= 0
            then errorString fns h
            else pure (Right (Image.ofRgb w ht (BI.fromForeignPtr fp 0 (w * ht * 3))))
