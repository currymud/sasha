{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Partitions.Verbs.ConsumptionVerbs
         (eat,take,consumptionVerbs,takeCV) where


import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Verbs                  (ConsumptionVerb (ConsumptionVerb))
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

makeSemanticValues [| ConsumptionVerb |] [EAT,TAKE]

consumptionVerbs :: HashSet ConsumptionVerb
consumptionVerbs = Data.HashSet.fromList [eat, take]

takeCV :: ConsumptionVerb
takeCV = Grammar.Parser.Partitions.Verbs.ConsumptionVerbs.take

#ifdef TESTING

instance Arbitrary ConsumptionVerb where
  arbitrary = elements consumptionVerbs

#endif
