module      Test.Parser.SpeechParts.Atomics.Adjectives where
import           Parser.SpeechParts.Atomics.Adjectives (adjectives)
import           Test.Hspec                            (Spec, describe, hspec)
import           Test.Hspec.QuickCheck                 (prop)
import           Test.Parser.SpeechParts.Atomics.Utils (checkLexeme)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Adjectives Arbitrary" do
  prop "Adjectives" $ checkLexeme adjectives
