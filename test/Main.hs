module Main (main) where

import Assert (finish, newSuite)
import qualified TestAsync
import qualified TestStreamDeck

main :: IO ()
main = do
  suite <- newSuite
  TestStreamDeck.tests suite
  TestAsync.tests suite
  finish suite
