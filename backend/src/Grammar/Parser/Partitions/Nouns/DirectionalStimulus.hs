{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Partitions.Nouns.DirectionalStimulus  where

import           Data.HashSet                                (HashSet, fromList)
import           Grammar.Parser.Partitions.Nouns.Utils       (lexemes)
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (DirectionalStimulus (DirectionalStimulus))
import           Model.Parser.Lexer                          (Lexeme (BEDROOM, CHAIR, FLOOR, KEY, MAIL, PILL, POCKET, ROBE, SATCHEL, TABLE, TOWEL))
import           Prelude                                     hiding (floor)
#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck                             (elements)
import           Test.QuickCheck.Arbitrary                   (Arbitrary (arbitrary))
#endif

makeSemanticValues [| DirectionalStimulus |] lexemes

directionalStimulii :: HashSet DirectionalStimulus
directionalStimulii = fromList [bedroom,pocket,pill,mail,satchel,towel,robe,chair, floor,table,key]

keyDS :: DirectionalStimulus
keyDS = key

bedroomDS :: DirectionalStimulus
bedroomDS = bedroom

chairDS :: DirectionalStimulus
chairDS = chair

tableDS :: DirectionalStimulus
tableDS = table

pillDS :: DirectionalStimulus
pillDS = pill

mailDS :: DirectionalStimulus
mailDS = mail

robeDS :: DirectionalStimulus
robeDS = robe

pocketDS :: DirectionalStimulus
pocketDS = pocket

floorDS :: DirectionalStimulus
floorDS = floor

#ifdef TESTING
instance Arbitrary DirectionalStimulus where
  arbitrary = elements $ HS.toList directionalStimulii
#endif
