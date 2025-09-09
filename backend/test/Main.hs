module Main where

import qualified Test.Categorical.MonadLaws               (spec)
import qualified Test.Categorical.FreeMonadLaws           (spec)
import qualified Test.Categorical.HierarchyNaturalityLaws (spec)
import qualified Test.Categorical.RegistryNaturalityLaws  (spec)
import qualified Test.Categorical.SpatialMonoidLaws       (spec)
import           Test.Hspec                               (hspec, runIO)
import qualified Test.Lexer
import qualified Test.Parser.Atomics.Verbs
main :: IO ()
main = hspec $ do
  runIO $ putStrLn "Running property tests..."
  Test.Lexer.spec
  Test.Parser.Atomics.Verbs.spec
  Test.Categorical.MonadLaws.spec
  Test.Categorical.FreeMonadLaws.spec
  Test.Categorical.HierarchyNaturalityLaws.spec
  Test.Categorical.RegistryNaturalityLaws.spec
  Test.Categorical.SpatialMonoidLaws.spec

