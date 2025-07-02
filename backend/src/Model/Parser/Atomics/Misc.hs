module Model.Parser.Atomics.Misc where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList, singleton,
                                            toList)
import           Data.Kind                 (Type)
import           Lexer
import           Relude.String.Conversion  (ToText)

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

type Partition :: Type
newtype Partition = Partition { _fromPartition :: Lexeme }
  deriving newtype (Show, Eq, Hashable, Ord)

instance HasLexeme Partition where
  toLexeme = _fromPartition

type Determiner :: Type
newtype Determiner =
  Determiner { _fromDeterminer :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Determiner where
  toLexeme = _fromDeterminer

#ifdef TESTING

instance Arbitrary Determiner where
  arbitrary = elements $ HS.toList determiners

#endif
