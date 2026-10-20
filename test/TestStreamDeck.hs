{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Tests for the synchronous library, driven through a fake transport.
module TestStreamDeck (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import Data.Bits ((.&.))
import Data.IORef
import Data.Word (Word8)

import Assert
import FakeTransport
import System.Hardware.StreamDeck
import qualified System.Hardware.StreamDeck.Bmp as Bmp
import qualified System.Hardware.StreamDeck.Image as Image
import qualified System.Hardware.StreamDeck.Jpeg as Jpeg
import qualified System.Hardware.StreamDeck.Util as Util

gradient :: Int -> Int -> Image
gradient w h = Image.generate w h $ \x y ->
  (byte8 (x * 7), byte8 (y * 11), byte8 (x + y))
  where
    byte8 :: Int -> Word8
    byte8 v = fromIntegral (v .&. 255)

tests :: Suite -> IO ()
tests suite = do
  test suite "kind round-trips through vendor and product id" $ do
    forM_ allKinds $ \k -> do
      check (kindToString k) (kindOfVidPid (vendorId k) (productId k) == Just k)
      check "string" (kindOfString (kindToString k) == Just k)
    check "unknown pid" (kindOfVidPid elgatoVendorId 0xffff == Nothing)
    check "foreign vendor" (kindOfVidPid 0x1234 (productId Mk2) == Nothing)

  test suite "key layout covers every key" $
    forM_ allKinds $ \k ->
      let (r, c) = keyLayout k in check (kindToString k) (r * c == keyCount k)

  test suite "flipKeyIndex mirrors columns and is an involution" $ do
    check "0 -> 4" (Util.flipKeyIndex Original 0 == 4)
    check "7 -> 7" (Util.flipKeyIndex Original 7 == 7)
    forM_ [0 .. 14] $ \k ->
      check "involution" (Util.flipKeyIndex Original (Util.flipKeyIndex Original k) == k)

  test suite "readButtonStates per family" $ do
    check "no data" (null (Util.readButtonStates Mk2 (bytes [0, 1, 1])))
    check "mini"
      (Util.readButtonStates Mini (bytes [1, 0, 1, 0, 0, 0, 1]) == [False, True, False, False, False, True])
    check "xl offset 4" (Util.readButtonStates Xl (bytes [1, 0, 0, 0, 1, 0]) == [True, False])
    let orig = Util.readButtonStates Original (bytes ([1, 1] ++ replicate 14 0))
    check "original flipped" (length orig == 15 && orig !! 4 && not (head orig))

  test suite "readLcdInput decodes press, long press, swipe" $ do
    let base t = bytes [1, 2, 0, 0, t, 0, 0x2c, 0x01, 0x32, 0, 0x10, 0, 0x20, 0]
    check "press" (Util.readLcdInput (base 1) == Right (InputTouchScreenPress 300 50))
    check "long" (Util.readLcdInput (base 2) == Right (InputTouchScreenLongPress 300 50))
    check "swipe" (Util.readLcdInput (base 3) == Right (InputTouchScreenSwipe (300, 50) (16, 32)))
    check "bad" (Util.readLcdInput (base 9) == Left BadData)

  test suite "readEncoderInput decodes signed twists" $ do
    check "twist"
      ( Util.readEncoderInput Plus (bytes [1, 3, 0, 0, 1, 0xff, 2, 0, 0x80])
          == Right (InputEncoderTwist [-1, 2, 0, -128])
      )
    check "press"
      ( Util.readEncoderInput Plus (bytes [1, 3, 0, 0, 0, 1, 0, 0, 1])
          == Right (InputEncoderStateChange [True, False, False, True])
      )

  test suite "extractStr strips NUL and rejects invalid UTF-8" $ do
    check "nul" (Util.extractStr (bytes [65, 66, 0, 67, 0]) == Right "ABC")
    check "utf8" (Util.extractStr (bytes [0xff, 0xfe]) == Left Utf8Error)

  test suite "imageDataReports pages, pads and flags the last page" $ do
    let d = B.pack [fromIntegral (i `mod` 251) | i <- [0 .. 2499 :: Int]]
        reports =
          imageDataReports d 1024 1016 $ \page thisLength lastPage ->
            [page, thisLength .&. 0xff, if lastPage then 1 else 0, 0, 0, 0, 0, 0]
    check "count" (length reports == 3)
    check "length" (all ((== 1024) . B.length) reports)
    check "last flag" (map (`B.index` 2) reports == [0, 0, 1])
    let payload =
          B.concat [B.take (min 1016 (2500 - i * 1016)) (B.drop 8 r) | (i, r) <- zip [0 ..] reports]
    check "payload" (payload == d)
    check "empty" (null (imageDataReports B.empty 1024 1016 (\_ _ _ -> [])))

  test suite "brightness clamps and uses family-specific report" $ do
    f <- newFake
    ok =<< (device Mini f >>= (`setBrightness` 150))
    mini <- head <$> readIORef (featuresSent f)
    check "mini len" (B.length mini == 17)
    check "mini value" (B.index mini 5 == 100)
    ok =<< (device Xl f >>= (`setBrightness` 40))
    xl <- head <$> readIORef (featuresSent f)
    check "xl len" (B.length xl == 32)
    check "xl value" (B.index xl 2 == 40)

  test suite "writeImage queues until flush" $ do
    f <- newFake
    d <- device Xl f
    ok =<< writeImage d 3 (B.replicate 2000 0x78)
    ok =<< writeImage d 5 (bytes [0x79])
    check "nothing sent" . null =<< readIORef (written f)
    ok =<< flush d
    sent <- reverse <$> readIORef (written f)
    check "pages" (length sent == 3)
    check "first key" (B.index (head sent) 2 == 3)
    check "second key" (B.index (sent !! 2) 2 == 5)
    writeIORef (written f) []
    ok =<< flush d
    check "cache cleared" . null =<< readIORef (written f)

  test suite "original key header flips index and splits image in halves" $ do
    f <- newFake
    ok =<< (device Original f >>= (`clearButtonImage` 0))
    sent <- reverse <$> readIORef (written f)
    check "two pages" (length sent == 2)
    check "report length" (all ((== 8191) . B.length) sent)
    check "page numbers" (map (`B.index` 2) sent == [1, 2])
    check "flipped key" (B.index (head sent) 5 == 5)

  test suite "operation errors" $ do
    f <- newFake
    mk2 <- device Mk2 f
    pedal <- device Pedal f
    xl <- device Xl f
    check "key" . (== Left InvalidKeyIndex) =<< clearButtonImage mk2 15
    check "pedal" . (== Left NoScreen) =<< clearButtonImage pedal 0
    check "lcd" . (== Left UnsupportedOperation) =<< writeLcdFill xl (bytes [0x78])
    check "touchpoint" . (== Left InvalidTouchPointIndex) =<< setTouchpointColor mk2 0 1 2 3

  test suite "touch point color addresses index after keys" $ do
    f <- newFake
    ok =<< (device Neo f >>= \d -> setTouchpointColor d 1 10 20 30)
    sent <- head <$> readIORef (featuresSent f)
    check "report" (sent == bytes [3, 6, 9, 10, 20, 30])

  test suite "serial number strips control bytes" $ do
    f <- newFake
    writeIORef (featureReply f) (bytes [0x06, 0x0c, 65, 66, 1, 49, 50, 0, 0])
    d <- device Xl f
    check "serial" . (== "AB12") =<< ok =<< serialNumber d
    check "manufacturer default" . (== "Unknown") =<< ok =<< manufacturer d

  test suite "reader turns states into updates" $ do
    f <- newFake
    r <- getReader =<< device Neo f
    writeIORef (queuedReads f)
      [ bytes [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1]
      , bytes [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
      ]
    check "down" . (== [ButtonDown 0, TouchPointDown 1]) =<< ok =<< readerRead r Nothing
    check "up" . (== [ButtonUp 0]) =<< ok =<< readerRead r Nothing
    check "no data" . null =<< ok =<< readerRead r Nothing

  test suite "plus reader dispatches encoder and lcd input" $ do
    f <- newFake
    r <- getReader =<< device Plus f
    writeIORef (queuedReads f)
      [ bytes [1, 3, 0, 0, 1, 0, 5]
      , bytes [1, 3, 0, 0, 0, 0, 1]
      , bytes [1, 2, 0, 0, 1, 0, 5, 0, 6, 0]
      , bytes [1, 7]
      ]
    check "twist" . (== [EncoderTwist 1 5]) =<< ok =<< readerRead r Nothing
    check "press" . (== [EncoderDown 1]) =<< ok =<< readerRead r Nothing
    check "lcd" . (== [TouchScreenPress 5 6]) =<< ok =<< readerRead r Nothing
    check "bad" . (== Left BadData) =<< readerRead r Nothing

  test suite "rotations and flips compose to identity" $ do
    let img = gradient 5 3
    check "rot90 dims" (Image.dimensions (Image.rotate90 img) == (3, 5))
    check "rot90 corner" (Image.getPixel (Image.rotate90 img) 2 0 == Image.getPixel img 0 0)
    check "rot90 x4" (iterate Image.rotate90 img !! 4 == img)
    check "rot90+rot270" (Image.rotate270 (Image.rotate90 img) == img)
    check "rot180" (Image.rotate180 img == Image.rotate90 (Image.rotate90 img))
    check "fliph" (Image.fliph (Image.fliph img) == img)
    check "flipv" (Image.getPixel (Image.flipv img) 0 0 == Image.getPixel img 0 2)

  test suite "resize keeps solid color and target size" $ do
    let img = Image.fill (Image.create 37 23) (12, 200, 99)
    forM_ [(72, 72), (5, 3), (37, 23)] $ \(w, h) -> do
      let r = Image.resizeExact img w h
      check "dims" (Image.dimensions r == (w, h))
      forM_ [(x, y) | y <- [0 .. h - 1], x <- [0 .. w - 1]] $ \(x, y) ->
        check "color" (Image.getPixel r x y == (12, 200, 99))

  test suite "resize averages neighbouring pixels" $ do
    -- Halving a two-column black/white image yields mid grey.
    let img = Image.generate 2 1 (\x _ -> if x == 0 then (0, 0, 0) else (255, 255, 255))
        r = Image.resizeExact img 1 1
    check "grey" (Image.getPixel r 0 0 == (128, 128, 128))

  test suite "bmp round-trip" $ do
    let img = gradient 7 4
        enc = Bmp.encode img
    check "size" (B.length enc == 54 + (24 * 4))
    check "decode" . (== img) =<< okS (Bmp.decode enc)

  test suite "jpeg round-trip keeps size and approximate color" $ do
    let img = Image.fill (Image.create 40 30) (200, 40, 90)
    dec <- okS =<< Jpeg.decode =<< okS =<< Jpeg.encode img
    check "dims" (Image.dimensions dec == (40, 30))
    let (r, g, b) = Image.getPixel dec 20 15
        near a e = abs (fromIntegral a - e :: Int) < 6
    check "color" (near r 200 && near g 40 && near b 90)

  test suite "convertImage follows the kind's key format" $ do
    let img = gradient 100 60
    forM_ allKinds $ \k -> do
      let fmt = keyImageFormat k
      d <- ok =<< convertImage k img
      case formatMode fmt of
        NoImage -> check "empty" (B.null d)
        _ -> do
          decoded <- ok =<< loadFromMemory d
          check (kindToString k) (Image.dimensions decoded == formatSize fmt)

  test suite "blank images decode to the key size" $
    forM_ allKinds $ \k ->
      case formatMode (keyImageFormat k) of
        Jpeg -> do
          decoded <- ok =<< loadFromMemory (blankImage k)
          check (kindToString k) (Image.dimensions decoded == formatSize (keyImageFormat k))
        _ -> pure ()

  test suite "example image loads" $ do
    img <- ok =<< loadFile "examples/no-place-like-localhost.jpg"
    check "non-empty" (imageWidth img > 0 && imageHeight img > 0)
