module Parser.PrepParser where
import           Data.Text                               (Text)
import           Lexer.Model                             (Lexeme)
import           Parser.Model.Prepositions               (PrepParsers (..))
import           Parser.SpeechParts                      (parseRule)
import           Parser.SpeechParts.Atomics.Prepositions (LocationInterrogativeMarker (..),
                                                          TargetedStimulusMarker (TargetedStimulusMarker),
                                                          locationInterrogativeMarker,
                                                          targetedStimulusMarker)
import           Text.Earley                             (Grammar)
import           Text.Earley.Grammar                     (Prod)

prepParser :: PrepParsers r
prepParser =
  let _locationInterrogativeMarker' :: Grammar r (Prod r Text Lexeme LocationInterrogativeMarker)
      _locationInterrogativeMarker' = parseRule locationInterrogativeMarker LocationInterrogativeMarker
      _targetedStimulusMarker' :: Grammar r (Prod r Text Lexeme TargetedStimulusMarker)
      _targetedStimulusMarker' = parseRule targetedStimulusMarker TargetedStimulusMarker
  in PrepParsers {..}
