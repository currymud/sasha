{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Partitions.Nouns.DirectionalStimulus  where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Nouns.Utils       (lexemes)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (DirectionalStimulus (DirectionalStimulus))
import           Model.Parser.Lexer                          (Lexeme (CHAIR, FLOOR, MAIL, PILL, POCKET, ROBE, SATCHEL, TABLE, TOWEL))
import           Prelude                                     hiding (floor)
#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| DirectionalStimulus |] lexemes

directionalStimulii :: HashSet DirectionalStimulus
directionalStimulii = fromList [pocket,pill,mail,satchel,towel,robe,chair, floor,table]

#ifdef TESTING
instance Arbitrary DirectionalStimulus where
  arbitary = elements $ HS.toList directionalStimulii
#endif
