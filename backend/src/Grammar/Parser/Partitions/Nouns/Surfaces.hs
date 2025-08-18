{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Grammar.Parser.Partitions.Nouns.Surfaces ( chairSF, tableSF, floorSF, surfaces,chair,table,satchel) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (CHAIR, FLOOR, SATCHEL, TABLE))
import           Grammar.Parser.Partitions.Nouns.Utils       (surfaces')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Surface (Surface))
import           Prelude                                     hiding (floor)
#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Surface |] surfaces'

surfaces :: HashSet Surface
surfaces = HashSet.fromList [chair,floor,table,satchel]

chairSF :: Surface
chairSF = chair

tableSF :: Surface
tableSF = table

floorSF :: Surface
floorSF = floor


#ifdef TESTING
instance Arbitrary Supportive where
  arbitary = elements $ HS.toList supportives
#endif
