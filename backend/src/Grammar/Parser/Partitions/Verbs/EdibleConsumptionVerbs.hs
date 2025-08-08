{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Partitions.Verbs.EdibleConsumptionVerbs
         (eat,take,edibleConsumptionVerbs) where


import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (EdibleConsumptionVerb (EdibleConsumptionVerb))
import           Model.Parser.Lexer                          (Lexeme (EAT, TAKE))
import           Prelude                                     hiding (take)
#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Data.Text                                   (Text)
import           Relude.String.Conversion                    (ToText (toText))
import           Test.QuickCheck                             (Arbitrary,
                                                              elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| EdibleConsumptionVerb |] [EAT,TAKE]

edibleConsumptionVerbs :: HashSet EdibleConsumptionVerb
edibleConsumptionVerbs = Data.HashSet.fromList [eat, take]

#ifdef TESTING

instance Arbitrary EdibleConsumptionVerb where
  arbitrary = elements edibleConsumptionVerbs

#endif
