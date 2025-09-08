module Grammar.Parser.Partitions.Nouns.SomaticStimulus (eyes,somaticStimulii) where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (SomaticStimulus (SomaticStimulus))
import           Model.Parser.Lexer                          (Lexeme (EYES))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck                             (elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| SomaticStimulus |] [EYES]

somaticStimulii :: HashSet SomaticStimulus
somaticStimulii = fromList [eyes]

#ifdef TESTING
instance Arbitrary SomaticStimulus where
  arbitrary = elements $ HS.toList somaticStimulii
#endif
