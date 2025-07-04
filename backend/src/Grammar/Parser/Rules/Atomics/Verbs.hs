module Grammar.Parser.Rules.Atomics.Verbs (implicitStimulusVerbRule) where
import           Data.Text                          (Text)
import           Grammar.Lexer                      (Lexeme)
import           Grammar.Model.Parser.Atomics.Verbs (ImplicitStimulusVerb (ImplicitStimulusVerb))
import           Grammar.Parser.Partitions.Verbs    (implicitStimulusVerbs)
import           Grammar.Parser.Rules.Atomics.Utils (parseRule)
import           Text.Earley.Grammar                (Grammar, Prod)

implicitStimulusVerbRule :: Grammar r (Prod r Text Lexeme ImplicitStimulusVerb)
implicitStimulusVerbRule = parseRule implicitStimulusVerbs ImplicitStimulusVerb
