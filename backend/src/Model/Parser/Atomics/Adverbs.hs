module Model.Parser.Atomics.Adverbs (ImplicitPath (ImplicitPath, _fromImplicitPath),
                                      NegativePosturalDirection (NegativePosturalDirection, _fromNegativePosturalDirection),
                                      PositivePosturalDirection (PositivePosturalDirection, _fromPositivePosturalDirection)) where

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

type NegativePosturalDirection :: Type
newtype NegativePosturalDirection = NegativePosturalDirection { _fromNegativePosturalDirection :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme NegativePosturalDirection where
  toLexeme = _fromNegativePosturalDirection

type PositivePosturalDirection :: Type
newtype PositivePosturalDirection = PositivePosturalDirection { _fromPositivePosturalDirection :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme PositivePosturalDirection where
  toLexeme = _fromPositivePosturalDirection

