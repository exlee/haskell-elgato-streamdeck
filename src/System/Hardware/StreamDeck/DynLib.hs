-- | Locates a shared library at runtime. An environment variable overrides
-- the candidate list.
module System.Hardware.StreamDeck.DynLib
  ( openFirst
  , symbol
  ) where

import Control.Exception (IOException, try)
import Data.List (intercalate)
import Foreign.Ptr (FunPtr)
import System.Environment (lookupEnv)
import System.Posix.DynamicLinker (DL, RTLDFlags (RTLD_NOW), dlopen, dlsym)

-- | Opens the first loadable library: the path in @env@ (when set and
-- non-empty) is tried before @candidates@.
openFirst :: String -> [String] -> IO (Either String DL)
openFirst env candidates0 = do
  override <- lookupEnv env
  let candidates = case override of
        Just path | not (null path) -> path : candidates0
        _ -> candidates0
      go errors [] =
        pure . Left $
          "cannot load any of [" ++ intercalate "; " candidates ++ "] (set "
            ++ env ++ "): " ++ intercalate "; " (reverse errors)
      go errors (name : rest) =
        try (dlopen name [RTLD_NOW]) >>= \case
          Right dl -> pure (Right dl)
          Left (e :: IOException) -> go (show e : errors) rest
  go [] candidates

-- | Looks up a symbol, reporting a missing one as an error message.
symbol :: DL -> String -> IO (Either String (FunPtr a))
symbol dl name =
  try (dlsym dl name) >>= \case
    Right f -> pure (Right f)
    Left (e :: IOException) -> pure (Left (name ++ ": " ++ show e))
