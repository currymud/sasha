module Parser.VerbParsers where
import           Data.Text                                (Text)
import           Lexer.Model                              (Lexeme)
import           Parser.Model.Nouns                       (NounParsers (..))
import           Parser.Model.Prepositions                (PrepParsers (..))
import           Parser.NounParsers                       (directionalStimulusNounParser,
                                                           modToggleNounPhraseParser,
                                                           simpleAccessNounPhraseParser,
                                                           toggleNounPhraseParser)
import           Parser.PhraseParsers                     (adjectivePhraseParser)
import           Parser.PrepParser                        (prepParser)
import           Parser.SpeechParts                       (parseRule)
import           Parser.SpeechParts.Atomics.Adverbs       (ImplicitPath (ImplicitPath),
                                                           implicitPaths)
import           Parser.SpeechParts.Atomics.Misc          (Determiner,
                                                           Partition (Partition),
                                                           determiners,
                                                           partitions)
import           Parser.SpeechParts.Atomics.Nouns         (NamedAgent (NamedAgent),
                                                           namedAgents)
import           Parser.SpeechParts.Atomics.Prepositions  (DirectionalStimulusMarker (..),
                                                           ObjectInterrogativeMarker (..),
                                                           SourceMarker (SourceMarker),
                                                           TopicMarker (TopicMarker),
                                                           directionalStimulusMarkers,
                                                           objectInterrogativeMarker,
                                                           sourceMarker,
                                                           topicMarker)
import           Parser.SpeechParts.Atomics.Verbs         (AcquisitionVerb (AcquisitionVerb),
                                                           CardinalMovementVerb (CardinalMovementVerb),
                                                           Copula (Copula),
                                                           DirectionalStimulusVerb (..),
                                                           ExplicitStimulusVerb (..),
                                                           GeneralPlacementVerb (GeneralPlacementVerb),
                                                           ImplicitRegionalStimulusVerb (..),
                                                           ImplicitStimulusVerb (ImplicitStimulusVerb),
                                                           ModToggleVerb (ModToggleVerb),
                                                           SimpleAccessVerb (SimpleAccessVerb),
                                                           TargetedStimulusVerb (TargetedStimulusVerb),
                                                           ToggleVerb (ToggleVerb),
                                                           acquisitionVerbs,
                                                           cardinalMovementVerbs,
                                                           copula,
                                                           directionalStimulusVerbs,
                                                           explicitStimulusVerbs,
                                                           generalPlacementVerbs,
                                                           implicitRegionalStimulusVerbs,
                                                           implicitStimulusVerbs,
                                                           modToggleVerbs,
                                                           simpleAccessVerbs,
                                                           targetedStimulusVerbs,
                                                           toggleVerbs)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase (AdjPhrase))
import           Parser.SpeechParts.Composites.Nouns      (ObjectPhrase,
                                                           SupportPhrase)
import           Parser.SpeechParts.Composites.Verbs      (AccessVerbPhrase,
                                                           AccessVerbPhraseRules (AccessVerbPhraseRules),
                                                           AcquisitionVerbPhrase,
                                                           AcquisitionVerbPhraseRules (AcquisitionVerbPhraseRules),
                                                           GeneralPlacementVerbPhrase,
                                                           GeneralPlacementVerbPhraseRules (GeneralPlacementVerbPhraseRules),
                                                           Imperative (..),
                                                           ImperativeRules (..),
                                                           Interrogative,
                                                           InterrogativeRules (InterrogativeRules),
                                                           StimulusVerbPhrase (..),
                                                           StimulusVerbPhraseRules (StimulusVerbPhraseRules),
                                                           Vocative,
                                                           VocativeRules (VocativeRules),
                                                           accessVerbPhraseRule,
                                                           acquisitionVerbPhraseRule,
                                                           generalPlacementVerbPhraseRule,
                                                           imperativeRule,
                                                           interrogativeRule,
                                                           stimulusVerbPhraseRule,
                                                           vocativeRule)
import           Text.Earley.Grammar                      (Grammar, Prod)

imperativePhraseParser :: NounParsers r
                            -> Prod r Text Lexeme Determiner
                            -> Prod r Text Lexeme AdjPhrase
                            -> Grammar r (Prod r Text Lexeme Imperative)
imperativePhraseParser nounParsers determiner adjPhrase = do
  _accessVerbPhrase <- accessVerbPhraseParser determiner adjPhrase
  _acquisitionVerbPhrase <- acquisitionVerbPhraseParser objectPhrase supportPhrase
  _generalPlacementVerbPhrase <- generalPlacementVerbPhraseParser objectPhrase supportPhrase
  _stimulusVerbPhrase <- stimulusVerbPhraseParser nounParsers determiner adjPhrase
  _cardinalMovementVerb <- parseRule cardinalMovementVerbs CardinalMovementVerb
  _implicitPath <- parseRule implicitPaths ImplicitPath
  imperativeRule $ ImperativeRules {..}
  where
    objectPhrase = nounParsers._objectPhrase'
    supportPhrase = nounParsers._supportPhrase'

interrogativePhraseParser :: NounParsers r
                            -> Grammar r (Prod r Text Lexeme Interrogative)
interrogativePhraseParser nounParsers = do
  objectInterrogativeMarker' <- parseRule objectInterrogativeMarker ObjectInterrogativeMarker
  topicMarker' <- parseRule topicMarker TopicMarker
  locationInterrogativeMarker' <- _locationInterrogativeMarker' prepParsers
  copula' <- parseRule copula Copula
  interrogativeRule $ InterrogativeRules
    objectInterrogativeMarker'
    topicMarker'
    locationInterrogativeMarker'
    copula'
    objectPhrase
  where
    objectPhrase = nounParsers._objectPhrase'
    prepParsers = prepParser

vocativeParser :: Prod r Text Lexeme Imperative
                    -> NounParsers r
                    -> Grammar r (Prod r Text Lexeme Vocative)
vocativeParser imperative nounParsers' = do
  namedAgent <- parseRule namedAgents NamedAgent -- does not belong in parser model
  partition <- parseRule partitions Partition
  interrogative <- interrogativePhraseParser nounParsers'
  vocativeRule $ VocativeRules namedAgent partition imperative interrogative

accessVerbPhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme AccessVerbPhrase)
accessVerbPhraseParser determiner adjPhrase = do
  toggleVerb <- parseRule toggleVerbs ToggleVerb
  toggleNounPhrase <- toggleNounPhraseParser determiner adjPhrase
  modToggleVerb' <- parseRule modToggleVerbs ModToggleVerb
  modToggleNounPhrase <- modToggleNounPhraseParser determiner adjPhrase
  simpleAccessVerb <- parseRule simpleAccessVerbs SimpleAccessVerb
  simpleAccessNounPhrase <- simpleAccessNounPhraseParser determiner adjPhrase
  let accessVerbPhraseRules = AccessVerbPhraseRules
                               toggleVerb
                               toggleNounPhrase
                               modToggleVerb'
                               modToggleNounPhrase
                               simpleAccessVerb
                               simpleAccessNounPhrase
  accessVerbPhraseRule accessVerbPhraseRules

acquisitionVerbPhraseParser :: Prod r Text Lexeme ObjectPhrase
                           -> Prod r Text Lexeme SupportPhrase
                           -> Grammar r (Prod r Text Lexeme AcquisitionVerbPhrase)
acquisitionVerbPhraseParser objectPhrase supportPhrase = do
  acquisitionVerb <- parseRule acquisitionVerbs AcquisitionVerb
  sourceMarker' <- parseRule sourceMarker SourceMarker
  acquisitionVerbPhraseRule
    $ AcquisitionVerbPhraseRules
        acquisitionVerb
        objectPhrase
        sourceMarker'
        supportPhrase

generalPlacementVerbPhraseParser :: Prod r Text Lexeme ObjectPhrase
                           -> Prod r Text Lexeme SupportPhrase
                           -> Grammar r (Prod r Text Lexeme GeneralPlacementVerbPhrase)
generalPlacementVerbPhraseParser objectPhrase supportPhrase = do
  generalPlacementVerb <- parseRule generalPlacementVerbs GeneralPlacementVerb
  generalPlacementVerbPhraseRule
    $ GeneralPlacementVerbPhraseRules generalPlacementVerb objectPhrase supportPhrase

stimulusVerbPhraseParser :: NounParsers r
                           -> Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseParser nounParsers determiner adjPhrase = do
  directionalStimulusNoun <- directionalStimulusNounParser determiner adjPhrase
  implicitStimulusVerb <- parseRule implicitStimulusVerbs ImplicitStimulusVerb
  explicitStimulusVerb <- parseRule explicitStimulusVerbs ExplicitStimulusVerb
  directionalStimulusVerb <- parseRule directionalStimulusVerbs DirectionalStimulusVerb
  directionalStimulusMarker <- parseRule directionalStimulusMarkers DirectionalStimulusMarker
  implicitRegionalStimulusVerb <- parseRule implicitRegionalStimulusVerbs ImplicitRegionalStimulusVerb
  targetedStimulusVerb <- parseRule targetedStimulusVerbs TargetedStimulusVerb
  stimulusVerbPhraseRule
    $ StimulusVerbPhraseRules
        implicitStimulusVerb
        explicitStimulusVerb
        targetedStimulusVerb
        targetedStimulusNounPhrase
        directionalStimulusVerb
        directionalStimulusMarker
        directionalStimulusNoun
        containerPhrase
        implicitRegionalStimulusVerb
  where
  targetedStimulusNounPhrase = _targetedStimulusNounPhrase' nounParsers
  containerPhrase = _containerPhrase' nounParsers



