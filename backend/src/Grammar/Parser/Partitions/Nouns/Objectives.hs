module Grammar.Parser.Partitions.Nouns.Objectives (objectives,pill,mail,satchel,towel,robe) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (MAIL, PILL, ROBE, SATCHEL, TOWEL))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Objective (Objective))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Objective |] [PILL,MAIL,SATCHEL,TOWEL,ROBE]

objectives :: HashSet Objective
objectives = HashSet.fromList [pill, mail, satchel, towel, robe]

#ifdef TESTING
instance Arbitrary Objective  where
  arbitary = elements $ HS.toList objectives
#endif
