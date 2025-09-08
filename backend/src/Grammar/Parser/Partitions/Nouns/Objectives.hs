{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Partitions.Nouns.Objectives ( floorOB,keyOB, bedroomOB,chairOB,pillOB,mailOB,pocketOB,tableOB,robeOB,floor,table,chair,objectives,pill,mail,satchel,towel,robe) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (BEDROOM, CHAIR, FLOOR, KEY, MAIL, PILL, POCKET, ROBE, SATCHEL, TABLE, TOWEL))
import           Grammar.Parser.Partitions.Nouns.Utils       (objectives')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Objective (Objective))

import           Prelude                                     hiding (floor)
#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck                             (elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| Objective |] objectives'

objectives :: HashSet Objective
objectives = HashSet.fromList [bedroom,pocket,floor,table,chair,pill, mail, satchel, towel, robe,key]

robeOB :: Objective
robeOB = robe

keyOB :: Objective
keyOB = key

floorOB :: Objective
floorOB = floor

bedroomOB :: Objective
bedroomOB = bedroom

chairOB :: Objective
chairOB = chair

tableOB :: Objective
tableOB = table

pillOB :: Objective
pillOB = pill

mailOB :: Objective
mailOB = mail

pocketOB :: Objective
pocketOB = pocket

#ifdef TESTING
instance Arbitrary Objective  where
  arbitrary = elements $ HS.toList objectives
#endif
