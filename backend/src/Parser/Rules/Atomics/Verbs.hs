module Parser.Rules.Atomics.Verbs (implicitStimulusVerbRule) where
import           Data.Text                  (Text)
import           Lexer                      (Lexeme)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb (ImplicitStimulusVerb))
import           Parser.Partitions.Verbs    (implicitStimulusVerbs)
import           Parser.Rules.Atomics.Utils (parseRule)
import           Text.Earley.Grammar        (Grammar, Prod)

implicitStimulusVerbRule :: Grammar r (Prod r Text Lexeme ImplicitStimulusVerb)
implicitStimulusVerbRule = parseRule implicitStimulusVerbs ImplicitStimulusVerb

