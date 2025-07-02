module Parser.Partitions.Misc where

import           Data.Hashable            (Hashable)
import           Data.HashSet             (HashSet, fromList, singleton, toList)
import           Data.Kind                (Type)
import           Lexer
import           Relude.String.Conversion (ToText)

lexemesHS :: HashSet Lexeme
lexemesHS = fromList ([minBound .. maxBound] :: [Lexeme])

partitions :: HashSet Partition
partitions = singleton $ Partition SEPERATOR

determiners :: HashSet Determiner
determiners = fromList $ map Determiner [THE, A, MY]

adjectives :: HashSet Adjective
adjectives = fromList $ map Adjective adjlex
  where adjlex = [BLUE, RED, GREAT, LONG, OLD, DRUNK, PLANT, POT, TEA
                 , CABINET, LOCKED, UNLOCKED, KITCHEN, LEFT, RIGHT
                 , FRONT, BEHIND, SMALL, LARGE]
