module Grammar.Parser.Rules.Atomics.Verbs (implicitStimulusVerbRule) where
import           Data.Text                                            (Text)
import           Grammar.Parser.Lexer                                 (Lexeme)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (implicitStimulusVerbs)
import           Grammar.Parser.Rules.Atomics.Utils                   (parseRule)
import           Model.Parser.Atomics.Verbs                           (ImplicitStimulusVerb (ImplicitStimulusVerb))
import           Text.Earley.Grammar                                  (Grammar,
                                                                       Prod)

implicitStimulusVerbRule :: Grammar r (Prod r Text Lexeme ImplicitStimulusVerb)
implicitStimulusVerbRule = parseRule implicitStimulusVerbs ImplicitStimulusVerb
