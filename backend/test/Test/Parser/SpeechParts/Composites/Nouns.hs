module      Test.Parser.SpeechParts.Composites.Nouns where

import           Parser.SpeechParts.Composites.Nouns (ContainerPhrase (..))
import           Prelude                             hiding (unwords)
import           Test.Hspec                          (Spec, hspec)
import           Test.QuickCheck.Arbitrary.Generic   (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen                 (Gen)

main :: IO ()
main = hspec spec

checkContainerPhrase :: Gen Bool
checkContainerPhrase = do
--  containerPhrase <- arbitrary :: Gen ContainerPhrase
  pure True

spec :: Spec
spec = pure ()
