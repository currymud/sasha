module Grammar.Parser.Partitions.Nouns.Supportives (supportives,chair,table,satchel) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (CHAIR, SATCHEL, TABLE))
import           Grammar.Parser.Partitions.Nouns.Utils       (supportives')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Supportive (Supportive))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Supportive |] supportives'

supportives :: HashSet Supportive
supportives = HashSet.fromList [chair,table,satchel]

#ifdef TESTING
instance Arbitrary Supportive where
  arbitary = elements $ HS.toList supportives
#endif
