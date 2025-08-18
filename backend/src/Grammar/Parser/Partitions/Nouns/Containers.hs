module Grammar.Parser.Partitions.Nouns.Containers (pocketCT, robeCT,containers,pocket,satchel,robe) where
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (POCKET, ROBE, SATCHEL))
import           Grammar.Parser.Partitions.Nouns.Utils       (containers')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Container (Container))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Container |] containers'

containers :: HashSet Container
containers = HashSet.fromList [pocket,satchel,robe]

robeCT :: Container
robeCT = robe

pocketCT :: Container
pocketCT = pocket

#ifdef TESTING
instance Arbitrary Container where
  arbitary = elements $ HS.toList containers
#endif
