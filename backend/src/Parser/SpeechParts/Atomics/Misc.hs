module Parser.SpeechParts.Atomics.Misc where

import           Data.Hashable (Hashable)
import           Data.HashSet  (HashSet, fromList, singleton, toList)
import           Data.Kind     (Type)
import           Lexer

lexemesHS :: HashSet Lexeme
lexemesHS = fromList ([minBound .. maxBound] :: [Lexeme])

type Partition :: Type
newtype Partition = Partition { _fromPartition :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme Partition where
  toLexeme = _fromPartition

partitions :: HashSet Partition
partitions = singleton $ Partition SEPERATOR

type Determiner :: Type
newtype Determiner =
  Determiner { _fromDeterminer :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable)

instance HasLexeme Determiner where
  toLexeme = _fromDeterminer

determiners :: HashSet Determiner
determiners = fromList $ map Determiner [THE, A, MY]
