module Grammar.Parser.Partitions.Adjectives (adjectives,plant,pot,white) where
import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Adjectives             (Adjective (Adjective))
import           Model.Parser.Lexer                          (Lexeme (PLANT, POT, WHITE))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
#endif

makeSemanticValues [| Adjective |] [PLANT,POT,WHITE]

adjectives :: HashSet Adjective
adjectives = fromList adjlex
  where adjlex = [plant, pot, white]

#ifdef TESTING
instance Arbitrary Adjective where
  arbitrary = elements $ HS.toList adjectives
#endif
