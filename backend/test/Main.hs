module Main where

import qualified Test.EndToEnd.GetRobeFails (spec)
import           Test.Hspec                 (hspec, runIO)
import qualified Test.Lexer
import qualified Test.Parser.Atomics.Verbs

main :: IO ()
main = do
  -- Run Hspec tests
  hspec $ do
    runIO $ putStrLn "Running property tests..."
    Test.Lexer.spec
    Test.Parser.Atomics.Verbs.spec
    Test.EndToEnd.GetRobeFails.spec
