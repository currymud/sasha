module Test.Parser.SpeechParts.Atomics.Nouns where
import           Parser.SpeechParts.Atomics.Nouns      (objectives)
import           Test.Hspec                            (Spec, describe, hspec)
import           Test.Hspec.QuickCheck                 (prop)
import           Test.Parser.SpeechParts.Atomics.Utils (checkLexeme)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Nouns Arbitrary" do
  prop "Objective" $ checkLexeme objectives
