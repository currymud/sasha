module Parser.PrepParser where
import           Data.Text                               (Text)
import           Lexer.Model                             (Lexeme)
import           Parser.Model.Prepositions               (PrepParsers (..))
import           Parser.SpeechParts                      (parseRule)
import           Parser.SpeechParts.Atomics.Prepositions (ContainmentMarker (..),
                                                          InstrumentalMarker (..),
                                                          LocationInterrogativeMarker (..),
                                                          Path (Path),
                                                          RecipientMarker (..),
                                                          SurfaceMarker (..),
                                                          TargetedStimulusMarker (TargetedStimulusMarker),
                                                          TraversalMarker (..),
                                                          containmentMarkers,
                                                          instrumentalMarker,
                                                          locationInterrogativeMarker,
                                                          paths,
                                                          recipientMarker,
                                                          surfaceMarkers,
                                                          targetedStimulusMarker,
                                                          traversalMarkers)
import           Text.Earley                             (Grammar)
import           Text.Earley.Grammar                     (Prod)

prepParser :: PrepParsers r
prepParser =
  let _locationInterrogativeMarker' :: Grammar r (Prod r Text Lexeme LocationInterrogativeMarker)
      _locationInterrogativeMarker' = parseRule locationInterrogativeMarker LocationInterrogativeMarker
      _targetedStimulusMarker' :: Grammar r (Prod r Text Lexeme TargetedStimulusMarker)
      _targetedStimulusMarker' = parseRule targetedStimulusMarker TargetedStimulusMarker
  in PrepParsers {..}

pathRule :: Grammar r (Prod r Text Lexeme Path)
pathRule = parseRule paths Path

surfaceMarkerRule :: Grammar r (Prod r Text Lexeme SurfaceMarker)
surfaceMarkerRule = parseRule surfaceMarkers SurfaceMarker

containmentMarkerRule :: Grammar r (Prod r Text Lexeme ContainmentMarker)
containmentMarkerRule = parseRule containmentMarkers ContainmentMarker

targetedStimulusMarkerRule :: Grammar r (Prod r Text Lexeme TargetedStimulusMarker)
targetedStimulusMarkerRule = parseRule targetedStimulusMarker TargetedStimulusMarker

instrumentalMarkerRule :: Grammar r (Prod r Text Lexeme InstrumentalMarker)
instrumentalMarkerRule = parseRule instrumentalMarker InstrumentalMarker

recipientMarkerRule :: Grammar r (Prod r Text Lexeme RecipientMarker)
recipientMarkerRule = parseRule recipientMarker RecipientMarker

traversalMarkerRule :: Grammar r (Prod r Text Lexeme TraversalMarker)
traversalMarkerRule = parseRule traversalMarkers TraversalMarker
