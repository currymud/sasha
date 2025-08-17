{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Partitions.Nouns.Objectives (robeOB,floor,table,chair,objectives,pill,mail,satchel,towel,robe) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (BEDROOM, CHAIR, FLOOR, MAIL, PILL, POCKET, ROBE, SATCHEL, TABLE, TOWEL))
import           Grammar.Parser.Partitions.Nouns.Utils       (objectives')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Objective (Objective))

import           Prelude                                     hiding (floor)
#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Objective |] objectives'

objectives :: HashSet Objective
objectives = HashSet.fromList [bedroom,pocket,floor,table,chair,pill, mail, satchel, towel, robe]

robeOB :: Objective
robeOB = Grammar.Parser.Partitions.Nouns.Objectives.robe

#ifdef TESTING
instance Arbitrary Objective  where
  arbitary = elements $ HS.toList objectives
#endif
