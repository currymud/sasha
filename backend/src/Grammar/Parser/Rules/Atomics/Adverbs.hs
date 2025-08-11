module Grammar.Parser.Rules.Atomics.Adverbs where
import           Data.Text                                            (Text)
import           Grammar.Parser.Lexer                                 (Lexeme)
import           Grammar.Parser.Partitions.Adverbs.ImplicitPaths      (implicitPaths)
import           Grammar.Parser.Partitions.Adverbs.PosturalDirections (negativePosturalDirections,
                                                                       positivePosturalDirections)
import           Grammar.Parser.Rules.Atomics.Utils                   (parseRule)
import           Model.Parser.Atomics.Adverbs                         (ImplicitPath (ImplicitPath),
                                                                       NegativePosturalDirection (NegativePosturalDirection),
                                                                       PositivePosturalDirection (PositivePosturalDirection))
import           Text.Earley.Grammar                                  (Grammar,
                                                                       Prod)

implicitPathRule :: Grammar r (Prod r Text Lexeme ImplicitPath)
implicitPathRule = parseRule implicitPaths ImplicitPath

positivePosturalDirectionRule :: Grammar r (Prod r Text Lexeme PositivePosturalDirection)
positivePosturalDirectionRule = parseRule positivePosturalDirections PositivePosturalDirection

negativePosturalDirectionRule :: Grammar r (Prod r Text Lexeme NegativePosturalDirection)
negativePosturalDirectionRule = parseRule negativePosturalDirections NegativePosturalDirection
