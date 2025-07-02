module Parser.Partitions.Adjectives where

import           Data.Hashable            (Hashable)
import           Data.HashSet             (HashSet, fromList)
import           Data.Kind                (Type)
import           Lexer
import           Relude.String.Conversion (ToText)

adjectives :: HashSet Adjective
adjectives = fromList $ map Adjective adjlex
  where adjlex = [BLUE, RED, GREAT, LONG, OLD, DRUNK, PLANT, POT, TEA
                 , CABINET, LOCKED, UNLOCKED, KITCHEN, LEFT, RIGHT
                 , FRONT, BEHIND, SMALL, LARGE]
