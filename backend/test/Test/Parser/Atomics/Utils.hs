module Test.Parser.Atomics.Utils where
import           Data.Text            (Text)
import           Grammar.Parser.Lexer (Lexeme)
import           Text.Earley.Grammar  (Grammar, Prod)
import           Text.Earley.Parser   (Parser, parser)

mkParser :: (forall r . Grammar r (Prod r Text Lexeme a))
              -> Parser Text [Lexeme] a
mkParser = parser
