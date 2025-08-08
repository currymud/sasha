module Grammar.Parser.Partitions.Nouns.Consumables (consumables, pill) where

import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import           Grammar.Parser.Lexer                        (Lexeme (PILL))
import           Grammar.Parser.Partitions.Nouns.Utils       (consumables')
import           Grammar.Parser.Partitions.Templates.Atomics (makeSemanticValues)
import           Model.Parser.Atomics.Nouns                  (Consumable (Consumable))

#ifdef TESTING
import qualified Data.HashSet                                as HS
import           Test.QuickCheck.Arbitrary                   (Arbitrary (Arbitrary, arbitrary),
                                                              elements)
#endif

makeSemanticValues [| Consumable |] consumables'

consumables :: HashSet Consumable
consumables = HashSet.fromList [pill]

#ifdef TESTING
instance Arbitrary Edible where
  arbitary = elements $ HS.toList consumables
#endif
