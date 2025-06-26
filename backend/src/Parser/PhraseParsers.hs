module Parser.PhraseParsers where
import           Data.Text                             (Text)
import           Lexer.Model                           (Lexeme)
import           Parser.SpeechParts                    (parseRule)
import           Parser.SpeechParts.Atomics.Adjectives (Adjective (Adjective),
                                                        adjectives)
import           Parser.SpeechParts.Atomics.Adverbs    (ImplicitPath (ImplicitPath),
                                                        implicitPaths)
import           Text.Earley.Grammar                   (Grammar, Prod)

implicitPathRule :: Grammar r (Prod r Text Lexeme ImplicitPath)
implicitPathRule = parseRule implicitPaths ImplicitPath
