module Grammar.Parser.Partitions.Nouns.DirectionalStimulus  where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Nouns.Utils       (lexemes)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (DirectionalStimulus (DirectionalStimulus))
import           Model.Parser.Lexer                          (Lexeme (CHAIR, MAIL, PILL, ROBE, SATCHEL, TABLE, TOWEL))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| DirectionalStimulus |] lexemes

directionalStimulii :: HashSet DirectionalStimulus
directionalStimulii = fromList [pill,mail,satchel,towel,robe,chair,table]

#ifdef TESTING
instance Arbitrary DirectionalStimulus where
  arbitary = elements $ HS.toList directionalStimulii
#endif
