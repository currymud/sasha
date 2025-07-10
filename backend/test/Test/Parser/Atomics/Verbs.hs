module Test.Parser.Atomics.Verbs where

import           Data.Text                                            (Text)
import           Grammar.Parser.Lexer                                 (Lexeme,
                                                                       lexify,
                                                                       tokens)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb
import           Grammar.Parser.Rules.Atomics.Verbs                   (implicitStimulusVerbRule)
import           Model.Parser.Atomics.Verbs                           (ImplicitStimulusVerb)
import           Relude.String                                        (ToText (toText))
import           Test.Hspec                                           (Spec,
                                                                       describe,
                                                                       hspec)
import           Test.Hspec.QuickCheck                                (prop)
import           Test.QuickCheck                                      (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen                                  (Gen)
import           Text.Earley.Parser                                   (Parser,
                                                                       fullParses,
                                                                       parser)

main :: IO ()
main = hspec spec


checkImplicitStimulusVerb :: Gen Bool
checkImplicitStimulusVerb = do
  implicitStimulusVerb :: ImplicitStimulusVerb <- arbitrary :: Gen ImplicitStimulusVerb

  case lexify tokens (toText implicitStimulusVerb) of
    Left _  -> pure False
    Right toks -> pure roundTrip
      where
        roundTrip = implicitStimulusVerb `elem` parsed
        implicitStimulusVerbParser :: Parser Text [Lexeme] ImplicitStimulusVerb
        implicitStimulusVerbParser = parser implicitStimulusVerbRule
        parsed = fst (fullParses implicitStimulusVerbParser toks)
  pure True

spec :: Spec
spec = describe "Atomic Verbs Roundtrips" $ do
  prop "Implicit Stimulus Verbs" checkImplicitStimulusVerb
