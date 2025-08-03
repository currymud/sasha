module Grammar.Parser.Partitions.Nouns.Surfaces (surfaces,chair,table,satchel) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (CHAIR, SATCHEL, TABLE))
import           Grammar.Parser.Partitions.Nouns.Utils       (surfaces')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Surface (Surface))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Surface |] surfaces'

surfaces :: HashSet Surface
surfaces = HashSet.fromList [chair,table,satchel]

#ifdef TESTING
instance Arbitrary Supportive where
  arbitary = elements $ HS.toList supportives
#endif
