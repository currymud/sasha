module Model.Parser.Atomics.Adverbs where

import           Data.Hashable             (Hashable)
import           Data.Kind                 (Type)
import           Model.Lexer               (Lexeme)
import           Relude.String.Conversion  (ToText)

#ifdef TESTING
import qualified Data.HashSet              as HS
import           Relude.String.Conversion  (ToText (toText))
import           Test.QuickCheck           (Arbitrary, elements)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
#endif

type ImplicitPath :: Type
newtype ImplicitPath = ImplicitPath { _fromImplicitPath :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

#ifdef TESTING

instance Arbitrary ImplicitPath where
  arbitrary = elements $ HS.toList implicitPaths

#endif
