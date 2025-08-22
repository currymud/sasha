module Grammar.Parser.Partitions.Verbs.AdministrativeVerbs where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (AdministrativeVerb (AdministrativeVerb))
import           Model.Parser.Lexer                          (Lexeme (DEBUG, QUIT))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| AdministrativeVerb |] [QUIT,DEBUG]

administrativeVerbs :: HashSet AdministrativeVerb
administrativeVerbs = fromList [quit,debug]

#ifdef TESTING

instance Arbitrary AdministrativeVerb where
  arbitrary = elements $ HS.toList administrativeVerbs
#endif

