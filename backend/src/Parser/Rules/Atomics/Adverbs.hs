module Parser.Rules.Atomics.Adverbs where
import           Data.Text                    (Text)
import           Lexer                        (Lexeme)
import           Model.Parser.Atomics.Adverbs (ImplicitPath (ImplicitPath))
import           Parser.Partitions.Adverbs    (implicitPaths)
import           Parser.Rules.Atomics.Utils   (parseRule)
import           Text.Earley.Grammar          (Grammar, Prod)

implicitPathRule :: Grammar r (Prod r Text Lexeme ImplicitPath)
implicitPathRule = parseRule implicitPaths ImplicitPath

