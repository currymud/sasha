module Grammar.Parser.Rules.Atomics.Prepositions where
import           Data.Text                          (Text)
import           Grammar.Parser.Rules.Atomics.Utils (parseRule)
import           Model.Parser.Atomics.Prepositions  (DirectionalStimulusMarker (DirectionalStimulusMarker))
import           Model.Parser.Lexer                 (Lexeme)
import           Text.Earley                        (Grammar, Prod)

directionalStimulusRule :: Grammar r (Prod r Text Lexeme DirectionalStimulusMarker)
directionalStimulusRule = parseRule directionalStimulusMarker DirectionalStimulusMarker
