module Grammar.Parser.Rules.Atomics.Prepositions (directionalStimulusMarkerRule,
                                                  instrumentMarkerRule) where
import           Data.Text                                                        (Text)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (directionalStimulusMarker)
import           Grammar.Parser.Partitions.Prepositions.InstrumentMarkers         (instrumentMarkers)
import           Grammar.Parser.Rules.Atomics.Utils                               (parseRule)
import           Model.Parser.Atomics.Prepositions                                (DirectionalStimulusMarker (DirectionalStimulusMarker),
                                                                                   InstrumentMarker (InstrumentMarker))
import           Model.Parser.Lexer                                               (Lexeme)
import           Text.Earley                                                      (Grammar,
                                                                                   Prod)

instrumentMarkerRule :: Grammar r (Prod r Text Lexeme InstrumentMarker)
instrumentMarkerRule =  parseRule instrumentMarkers InstrumentMarker

directionalStimulusMarkerRule :: Grammar r (Prod r Text Lexeme DirectionalStimulusMarker)
directionalStimulusMarkerRule =
  parseRule directionalStimulusMarker DirectionalStimulusMarker
