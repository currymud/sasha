module Test.Parser.Atomics.Utils where
import           Data.Hashable       (Hashable)
import           Data.Text           (Text)
import           Lexer               (HasLexeme, Lexeme)
import           Text.Earley.Grammar (Grammar, Prod)
import           Text.Earley.Parser  (Parser, parser)

-- mkParser :: (HasLexeme a, Hashable a)
--               => Grammar r (Prod r Text Lexeme a)
--               -> Parser Text Lexeme a
mkParser rule = parser rule
