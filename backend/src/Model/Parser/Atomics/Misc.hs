module Model.Parser.Atomics.Misc (Determiner (Determiner,_fromDeterminer)) where
import           Data.Hashable            (Hashable)
import           Data.Kind                (Type)
import           Grammar.Parser.Lexer     (HasLexeme (toLexeme))
import           Model.Parser.Lexer       (Lexeme)
import           Relude.String.Conversion (ToText)

type Determiner :: Type
newtype Determiner =
  Determiner { _fromDeterminer :: Lexeme }
  deriving stock (Show,Eq,Ord)
  deriving newtype (Hashable,ToText)

instance HasLexeme Determiner where
  toLexeme = _fromDeterminer

