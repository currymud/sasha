module Main where

import           Test.Hspec                (hspec, runIO)
import qualified Test.Lexer
import qualified Test.Parser.Atomics.Verbs

main :: IO ()
main = hspec $ do
  runIO $ putStrLn "Running property tests..."
  Test.Lexer.spec
  Test.Parser.Atomics.Verbs.spec
