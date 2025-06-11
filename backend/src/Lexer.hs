module Lexer (
    Lexer.Model.Lexeme (..)
  , Lexer.HasLexeme (..)
  , Lexer.runParser
  , Lexer.tokens
  ) where
import           Data.Char                (toUpper)
import           Data.Kind                (Constraint, Type)
import           Data.Text                (Text)
import           Lexer.Model
import           Relude.String.Conversion (ToString (toString), ToText (toText))
import           Text.Megaparsec          hiding (runParser)

runParser :: Parser a -> Text -> Either Text a
runParser parser str = do
  case parse parser "" str' of
    Left err   -> Left $ toText $ errorBundlePretty err
    Right prog -> Right prog
  where
    str' = map toUpper $ toString str

tokens :: Parser [Lexeme]
tokens = sc *> many term <* eof

type HasLexeme :: Type -> Constraint
class HasLexeme a where
  toLexeme :: a -> Lexeme
