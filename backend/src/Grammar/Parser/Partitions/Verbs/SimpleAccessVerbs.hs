module Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (SimpleAccessVerb (SimpleAccessVerb))
import           Model.Parser.Lexer                          (Lexeme (CLOSE, OPEN))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| SimpleAccessVerb |] [OPEN, CLOSE]
-- These verbs don't take instruments
simpleAccessVerbs :: HashSet SimpleAccessVerb
simpleAccessVerbs = fromList [open,close]

#ifdef TESTING

instance Arbitrary SimpleAccessVerb where
  arbitrary = elements $ HS.toList simpleAccessVerbs

#endif

