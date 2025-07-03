module Main where

import           Test.Hspec (hspec, runIO)
import qualified Test.Lexer

main :: IO ()
main = hspec $ do
  runIO $ putStrLn "Running property tests..."
  Test.Lexer.spec
