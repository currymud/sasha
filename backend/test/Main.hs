module Main where

import qualified Test.EndToEnd.GetRobeFails           (spec)
import qualified Test.EndToEnd.LookAtRobeProgression  (spec)
import qualified Test.EndToEnd.OpenEyesTriggerTest    (spec)
import qualified Test.EndToEnd.OpenEyesUnlocksGetRobe (spec)
import           Test.Hspec                           (hspec, runIO)
import qualified Test.Lexer
import qualified Test.Parser.Atomics.Verbs
main :: IO ()
main = do
  -- Run Hspec tests
  hspec $ do
    runIO $ putStrLn "Running property tests..."
--    Test.Lexer.spec
--    Test.Parser.Atomics.Verbs.spec
    Test.EndToEnd.GetRobeFails.spec
    Test.EndToEnd.LookAtRobeProgression.spec
--    Test.EndToEnd.OpenEyesTriggerTest.spec
--    Test.EndToEnd.OpenEyesUnlocksGetRobe.spec
