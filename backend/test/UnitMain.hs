module Main where

import           Test.Hspec                            (hspec, runIO)
import qualified Test.Parser.SpeechParts.Atomics.Verbs

main :: IO ()
main = hspec $ do
  runIO $ putStrLn "Running unit tests..."
  Test.Parser.SpeechParts.Atomics.Verbs.spec
  pure ()
