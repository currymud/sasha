module Grammar.Parser.Partitions.Nouns.Objectives (table,chair,objectives,pill,mail,satchel,towel,robe) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (CHAIR, MAIL, PILL, POCKET, ROBE, SATCHEL, TABLE, TOWEL))
import           Grammar.Parser.Partitions.Nouns.Utils       (objectives')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Objective (Objective))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Objective |] objectives'

objectives :: HashSet Objective
objectives = HashSet.fromList [table,chair,pill, mail, satchel, towel, robe]

#ifdef TESTING
instance Arbitrary Objective  where
  arbitary = elements $ HS.toList objectives
#endif
