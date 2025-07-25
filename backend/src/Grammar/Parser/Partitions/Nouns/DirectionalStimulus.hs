module Grammar.Parser.Partitions.Nouns.DirectionalStimulus  where
import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Nouns.Utils       (lexemes)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (DirectionalStimulus (DirectionalStimulus))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif
  {-
directionalStimulii :: HashSet DirectionalStimulus
directionalStimulii = fromList $ DirectionalStimulus <$>
-}
makeSemanticValues [| DirectionalStimulus |] lexemes

#ifdef TESTING
instance Arbitrary DirectionalStimulus where
  arbitary = elements $ HS.toList directionalStimulii
#endif
