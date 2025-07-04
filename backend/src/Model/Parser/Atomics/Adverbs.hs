module Model.Parser.Atomics.Adverbs where

import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           Grammar.Parser.Lexer     (HasLexeme (toLexeme))
import           Model.Parser.Lexer       (Lexeme)
import           Relude.String.Conversion (ToText)

type ImplicitPath :: Type
newtype ImplicitPath = ImplicitPath { _fromImplicitPath :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme ImplicitPath where
  toLexeme = _fromImplicitPath
