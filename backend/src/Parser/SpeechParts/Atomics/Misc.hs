module Parser.SpeechParts.Atomics.Misc where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList, singleton,
                                            toList)
import           Data.Kind                 (Type)
import           Lexer

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

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

#ifdef TESTING

instance Arbitrary Determiner where
  arbitrary = elements $ HS.toList determiners

#endif
