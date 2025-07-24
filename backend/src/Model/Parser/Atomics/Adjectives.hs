module Model.Parser.Atomics.Adjectives where
import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           Grammar.Parser.Lexer     (HasLexeme (toLexeme))
import           Model.Parser.Lexer       (Lexeme)
import           Relude.String.Conversion (ToText)

type Adjective :: Type
newtype Adjective =
  Adjective { _fromAdjective :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Adjective where
  toLexeme = _fromAdjective
