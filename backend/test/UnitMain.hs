module Main where

import           Test.Hspec (hspec, runIO)

main :: IO ()
main = hspec $ do
  runIO $ putStrLn "Running unit tests..."
  pure ()
