module Grammar.Parser.Rules.Atomics.Adverbs where
import           Data.Text                            (Text)
import           Grammar.Lexer                        (Lexeme)
import           Grammar.Model.Parser.Atomics.Adverbs (ImplicitPath (ImplicitPath))
import           Grammar.Parser.Partitions.Adverbs    (implicitPaths)
import           Grammar.Parser.Rules.Atomics.Utils   (parseRule)
import           Text.Earley.Grammar                  (Grammar, Prod)

implicitPathRule :: Grammar r (Prod r Text Lexeme ImplicitPath)
implicitPathRule = parseRule implicitPaths ImplicitPath

