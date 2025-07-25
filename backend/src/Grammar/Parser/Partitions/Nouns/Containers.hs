module Grammar.Parser.Partitions.Nouns.Containers (containers,satchel,robe) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (ROBE, SATCHEL))
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Container (Container))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Container |] [SATCHEL,ROBE]

containers :: HashSet Container
containers = HashSet.fromList [satchel,robe]


#ifdef TESTING
instance Arbitrary Container where
  arbitary = elements $ HS.toList containers
#endif
