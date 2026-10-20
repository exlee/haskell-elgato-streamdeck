-- | Errors, device input and state updates.
module System.Hardware.StreamDeck.Types
  ( Error (..)
  , errorToString
  , Input (..)
  , inputIsEmpty
  , Update (..)
  , updateToString
  ) where

-- | Errors that can occur while working with Stream Decks.
data Error
  = HidError String
  | Utf8Error
  | ImageError String
  | NoScreen
  | InvalidKeyIndex
  | InvalidTouchPointIndex
  | UnrecognizedPid
  | UnsupportedOperation
  | BadData
  deriving (Eq, Show)

errorToString :: Error -> String
errorToString = \case
  HidError msg -> "HidError(" ++ msg ++ ")"
  Utf8Error -> "Utf8Error"
  ImageError msg -> "ImageError(" ++ msg ++ ")"
  NoScreen -> "NoScreen"
  InvalidKeyIndex -> "InvalidKeyIndex"
  InvalidTouchPointIndex -> "InvalidTouchPointIndex"
  UnrecognizedPid -> "UnrecognizedPID"
  UnsupportedOperation -> "UnsupportedOperation"
  BadData -> "BadData"

-- | Type of input that the device produced. Constructors carry an @Input@
-- prefix because 'Update' shares the remaining names.
data Input
  = InputNoData
  | InputButtonStateChange [Bool]
  | InputEncoderStateChange [Bool]
  | InputEncoderTwist [Int]
  | InputTouchScreenPress Int Int
  | InputTouchScreenLongPress Int Int
  | InputTouchScreenSwipe (Int, Int) (Int, Int)
  deriving (Eq, Show)

inputIsEmpty :: Input -> Bool
inputIsEmpty InputNoData = True
inputIsEmpty _ = False

-- | What changed in device state.
data Update
  = ButtonDown Int
  | ButtonUp Int
  | EncoderDown Int
  | EncoderUp Int
  | EncoderTwist Int Int
  | TouchPointDown Int
  | TouchPointUp Int
  | TouchScreenPress Int Int
  | TouchScreenLongPress Int Int
  | TouchScreenSwipe (Int, Int) (Int, Int)
  deriving (Eq, Ord, Show)

updateToString :: Update -> String
updateToString = \case
  ButtonDown k -> "ButtonDown(" ++ show k ++ ")"
  ButtonUp k -> "ButtonUp(" ++ show k ++ ")"
  EncoderDown k -> "EncoderDown(" ++ show k ++ ")"
  EncoderUp k -> "EncoderUp(" ++ show k ++ ")"
  EncoderTwist k t -> "EncoderTwist(" ++ show k ++ ", " ++ show t ++ ")"
  TouchPointDown k -> "TouchPointDown(" ++ show k ++ ")"
  TouchPointUp k -> "TouchPointUp(" ++ show k ++ ")"
  TouchScreenPress x y -> "TouchScreenPress(" ++ show x ++ ", " ++ show y ++ ")"
  TouchScreenLongPress x y ->
    "TouchScreenLongPress(" ++ show x ++ ", " ++ show y ++ ")"
  TouchScreenSwipe (sx, sy) (ex, ey) ->
    "TouchScreenSwipe((" ++ show sx ++ ", " ++ show sy ++ "), ("
      ++ show ex ++ ", " ++ show ey ++ "))"
