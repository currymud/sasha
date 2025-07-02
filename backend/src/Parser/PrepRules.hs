module Parser.PrepRules where
import           Data.Text                            (Text)
import           Lexer.Model                          (Lexeme)
import           Model.Parser.Atomics.Adjectives      (Adjective)
import           Model.Parser.Atomics.Misc            (Determiner)
import           Model.Parser.Atomics.Prepositions    (ContainmentMarker (..),
                                                       InstrumentalMarker (..),
                                                       LocationInterrogativeMarker (..),
                                                       ObjectInterrogativeMarker (..),
                                                       Path (Path),
                                                       RecipientMarker (..),
                                                       SourceMarker (SourceMarker),
                                                       SurfaceMarker (..),
                                                       TargetedStimulusMarker (TargetedStimulusMarker),
                                                       TraversalMarker (..),
                                                       containmentMarkers,
                                                       instrumentalMarker,
                                                       locationInterrogativeMarker,
                                                       objectInterrogativeMarker,
                                                       paths, recipientMarker,
                                                       sourceMarker,
                                                       surfaceMarkers,
                                                       targetedStimulusMarker,
                                                       traversalMarkers)
import           Model.Parser.Prepositions            (PrepParsers (..))
import           Model.Parser.SpeechParts             (parseRule)

import           Model.Parser.Composites.Prepositions (InstrumentMarkerPhrase,
                                                       InstrumentMarkerPhraseRules (InstrumentMarkerPhraseRules))
import qualified Model.Parser.Composites.Prepositions (instrumentMarkerPhraseRules)
import           Text.Earley                          (Grammar)
import           Text.Earley.Grammar                  (Prod)

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

sourceMarkerRule :: Grammar r (Prod r Text Lexeme SourceMarker)
sourceMarkerRule = parseRule sourceMarker SourceMarker

containmentMarkerRule :: Grammar r (Prod r Text Lexeme ContainmentMarker)
containmentMarkerRule = parseRule containmentMarkers ContainmentMarker

targetedStimulusMarkerRule :: Grammar r (Prod r Text Lexeme TargetedStimulusMarker)
targetedStimulusMarkerRule = parseRule targetedStimulusMarker TargetedStimulusMarker

instrumentalMarkerRule :: Grammar r (Prod r Text Lexeme InstrumentalMarker)
instrumentalMarkerRule = parseRule instrumentalMarker InstrumentalMarker

recipientMarkerRule :: Grammar r (Prod r Text Lexeme RecipientMarker)
recipientMarkerRule = parseRule recipientMarker RecipientMarker

objectInterrogativeMarkerRule :: Grammar r (Prod r Text Lexeme ObjectInterrogativeMarker)
objectInterrogativeMarkerRule = parseRule objectInterrogativeMarker ObjectInterrogativeMarker

traversalMarkerRule :: Grammar r (Prod r Text Lexeme TraversalMarker)
traversalMarkerRule = parseRule traversalMarkers TraversalMarker

instrumentMarkerPhraseRules :: Grammar r (Prod r Text Lexeme Determiner)
                            -> Grammar r (Prod r Text Lexeme Adjective)
                                 -> Grammar r (Prod r Text Lexeme InstrumentMarkerPhrase)
instrumentMarkerPhraseRules determiner adj = do
  objectPathPhraseRules' <- objectPathPhraseRules determiner adj
  instrumentMarkerRules' <- instrumentalMarkerRule
  Parser.SpeechParts.Composites.Prepositions.instrumentMarkerPhraseRules $
    InstrumentMarkerPhraseRules
      instrumentMarkerRules'
      objectPathPhraseRules'
 -- InstrumentMarkerPhraseRules r

