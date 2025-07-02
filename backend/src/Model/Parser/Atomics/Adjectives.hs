module Model.Parser.Atomics.Adjectives where

import           Data.Hashable             (Hashable)
import           Data.HashSet              (HashSet, fromList)
import           Data.Kind                 (Type)
import           Lexer
import           Relude.String.Conversion  (ToText)

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

-- Need to distinguish between adjecti ves that can describe objects and those exclusively for people
-- e.g. "drunk" can describe a person but not an object

type Adjective :: Type
newtype Adjective =
  Adjective { _fromAdjective :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Adjective where
  toLexeme = _fromAdjective

#ifdef TESTING
instance Arbitrary Adjective where
  arbitrary = elements $ HS.toList adjectives
#endif
