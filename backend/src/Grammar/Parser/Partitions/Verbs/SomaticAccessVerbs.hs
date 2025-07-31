module Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (SomaticAccessVerb (SomaticAccessVerb))
import           Model.Parser.Lexer                          (Lexeme (CLOSE, OPEN))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| SomaticAccessVerb |] [OPEN, CLOSE]
-- These verbs don't take instruments
somaticAccessVerbs :: HashSet SomaticAccessVerb
somaticAccessVerbs = fromList [open, close]

#ifdef TESTING

instance Arbitrary SomaticAccessVerb where
  arbitrary = elements $ HS.toList somaticAccessVerbs

#endif

