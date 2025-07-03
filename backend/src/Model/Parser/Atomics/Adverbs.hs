module Model.Parser.Atomics.Adverbs where

import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           Model.Lexer              (Lexeme)
import           Relude.String.Conversion (ToText)

type ImplicitPath :: Type
newtype ImplicitPath = ImplicitPath { _fromImplicitPath :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)
