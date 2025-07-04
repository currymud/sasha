module Grammar.Parser.Rules.Atomics.Adverbs where
import           Data.Text                          (Text)
import           Grammar.Parser.Lexer               (Lexeme)
import           Grammar.Parser.Partitions.Adverbs  (implicitPaths)
import           Grammar.Parser.Rules.Atomics.Utils (parseRule)
import           Model.Parser.Atomics.Adverbs       (ImplicitPath (ImplicitPath))
import           Text.Earley.Grammar                (Grammar, Prod)

implicitPathRule :: Grammar r (Prod r Text Lexeme ImplicitPath)
implicitPathRule = parseRule implicitPaths ImplicitPath

