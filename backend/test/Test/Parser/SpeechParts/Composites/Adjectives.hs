module      Test.Parser.SpeechParts.Composites.Adjectives where

import           Data.Text                                (unwords)
import           Lexer                                    (runParser, tokens)
import           Parser.PhraseParsers                     (adjPhraseRule)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase (..))
import           Prelude                                  hiding (unwords)
import           Relude.String.Conversion                 (ToText (toText))
import           Test.Hspec                               (Spec, describe,
                                                           hspec)
import           Test.Hspec.QuickCheck                    (prop)
import           Test.QuickCheck.Arbitrary.Generic        (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen                      (Gen)
import           Text.Earley.Parser                       (fullParses, parser)

main :: IO ()
main = hspec spec

checkAdjPhrase :: Gen Bool
checkAdjPhrase = do
  adjPhrase <- arbitrary :: Gen AdjPhrase
  pure $ case adjPhrase of
    SimpleAdjPhrase adj -> case runParser tokens (toText adj) of
                              Left _ -> False
                              Right toks -> roundTrip
                                            where
                                            roundTrip =
                                              adjPhrase `elem` parsed toks

    AdjPhrase adj1 adj2 -> case runParser tokens textify of
                              Left _     -> False
                              Right toks -> adjPhrase `elem` parsed toks
                              where
                                textify = unwords [toText adj1,toText adj2]
  where
    adjParser = parser adjPhraseRule
    parsed toks = fst (fullParses adjParser toks)

spec :: Spec
spec = describe "AdjPhrase Arbitrary" do
  prop "Adj round tripping"  checkAdjPhrase
