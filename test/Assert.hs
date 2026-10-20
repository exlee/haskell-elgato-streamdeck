-- | Tiny assertion helpers: named tests that report and count failures.
module Assert
  ( Suite
  , newSuite
  , test
  , check
  , ok
  , okS
  , finish
  ) where

import Control.Exception (SomeException, displayException, throwIO, try, ErrorCall (..))
import Control.Monad (unless, when)
import Data.IORef
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

import System.Hardware.StreamDeck (Error, errorToString)

newtype Suite = Suite (IORef Int)

newSuite :: IO Suite
newSuite = Suite <$> newIORef 0

-- | Runs a test; an exception marks it failed.
test :: Suite -> String -> IO () -> IO ()
test (Suite failures) name body = do
  r <- try body
  case r of
    Right () -> putStrLn ("ok   " ++ name)
    Left (e :: SomeException) -> do
      modifyIORef' failures (+ 1)
      putStrLn ("FAIL " ++ name ++ ": " ++ displayException e)
  hFlush stdout

check :: String -> Bool -> IO ()
check name cond = unless cond (throwIO (ErrorCall name))

ok :: Either Error a -> IO a
ok = either (throwIO . ErrorCall . errorToString) pure

okS :: Either String a -> IO a
okS = either (throwIO . ErrorCall) pure

-- | Exits with failure when any test failed.
finish :: Suite -> IO ()
finish (Suite failures) = do
  n <- readIORef failures
  when (n > 0) $ do
    putStrLn (show n ++ " failure(s)")
    exitFailure
