module Grammar.Parser.Partitions.Verbs.PosturalVerbs where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (NegativePosturalVerb (NegativePosturalVerb),
                                                              PositivePosturalVerb (PositivePosturalVerb))
import           Model.Parser.Lexer                          (Lexeme (SIT, STAND))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| PositivePosturalVerb |] [STAND]
makeSemanticValues [| NegativePosturalVerb |] [SIT]

positivePosturalVerbs :: HashSet PositivePosturalVerb
positivePosturalVerbs = fromList [stand]

negativePosturalVerbs :: HashSet NegativePosturalVerb
negativePosturalVerbs = fromList [sit]

#ifdef TESTING

instance Arbitrary PositivePosturalVerb where
  arbitrary = elements $ HS.toList positivePosturalVerbs

instance Arbitrary NegativePosturalVerb where
  arbitrary = elements $ HS.toList negativePosturalVerbs


#endif

