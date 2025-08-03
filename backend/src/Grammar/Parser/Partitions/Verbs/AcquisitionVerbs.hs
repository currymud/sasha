module Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (acquisitionVerbs,get,take) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (AcquisitionVerb (AcquisitionVerb))
import           Model.Parser.Lexer                          (Lexeme (GET))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| AcquisitionVerb |] [GET]

acquisitionVerbs :: HashSet AcquisitionVerb
acquisitionVerbs = fromList [get]

#ifdef TESTING

instance Arbitrary AcquisitionVerb where
  arbitrary = elements $ HS.toList acquisitionVerbs

#endif

