module Grammar.Parser.Partitions.Verbs.EdibleConsumptionVerbs
         (eat,edibleConsumptionVerbs) where


import           Data.HashSet                                (HashSet,
                                                              singleton)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (EdibleConsumptionVerb (EdibleConsumptionVerb))
import           Model.Parser.Lexer                          (Lexeme (EAT))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| EdibleConsumptionVerb |] [EAT]

edibleConsumptionVerbs :: HashSet EdibleConsumptionVerb
edibleConsumptionVerbs = singleton eat

#ifdef TESTING

instance Arbitrary EdibleConsumptionVerb where
  arbitrary = elements edibleConsumptionVerbs

#endif
