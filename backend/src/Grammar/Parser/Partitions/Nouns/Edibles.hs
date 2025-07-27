module Grammar.Parser.Partitions.Nouns.Edibles (edibles, pill) where

import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (PILL))
import           Grammar.Parser.Partitions.Nouns.Utils       (edibles')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Edible (Edible))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Edible |] edibles'

edibles :: HashSet Edible
edibles = HashSet.fromList [pill]

#ifdef TESTING
instance Arbitrary Edible where
  arbitary = elements $ HS.toList edibles
#endif
