module Main where

import           Test.Hspec                                      (hspec, runIO)
import qualified Test.Lexer
import qualified Test.Parser.SpeechParts.Atomics.Adjectives
import qualified Test.Parser.SpeechParts.Atomics.Nouns
import qualified Test.Parser.SpeechParts.Atomics.Verbs
import qualified Test.Parser.SpeechParts.Composites.Adjectives
import qualified Test.Parser.SpeechParts.Composites.Nouns
import qualified Test.Parser.SpeechParts.Composites.Prepositions
main :: IO ()
main = hspec $ do
  runIO $ putStrLn "Running property tests..."
  Test.Parser.SpeechParts.Atomics.Verbs.spec
  Test.Parser.SpeechParts.Atomics.Nouns.spec
  Test.Parser.SpeechParts.Atomics.Adjectives.spec
  Test.Parser.SpeechParts.Composites.Adjectives.spec
  Test.Parser.SpeechParts.Composites.Nouns.spec
  Test.Parser.SpeechParts.Composites.Prepositions.spec
  runIO $ putStrLn "Running unit tests..."
  Test.Lexer.spec

  pure ()
