module Grammar.Parser.Rules.Atomics.Prepositions (directionalStimulusMarkerRule) where
import           Data.Text                                                        (Text)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (directionalStimulusMarker)
import           Grammar.Parser.Rules.Atomics.Utils                               (parseRule)
import           Model.Parser.Atomics.Prepositions                                (DirectionalStimulusMarker (DirectionalStimulusMarker))
import           Model.Parser.Lexer                                               (Lexeme)
import           Text.Earley                                                      (Grammar,
                                                                                   Prod)

directionalStimulusMarkerRule :: Grammar r (Prod r Text Lexeme DirectionalStimulusMarker)
directionalStimulusMarkerRule =
  parseRule directionalStimulusMarker DirectionalStimulusMarker
