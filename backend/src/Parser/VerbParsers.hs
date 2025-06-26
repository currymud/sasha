module Parser.VerbParsers where
import           Data.Text                               (Text)
import           Lexer.Model                             (Lexeme)
import           Parser.Model.Nouns                      (NounRules (..))
import           Parser.Model.Prepositions               (PrepParsers (..))
import           Parser.NounRules                        (directionalStimulusNounParser,
                                                          modToggleNounPhraseParser,
                                                          simpleAccessNounPhraseParser,
                                                          toggleNounPhraseParser)
import           Parser.PrepParser                       (prepParser)
import           Parser.SpeechParts                      (implicitPathRule,
                                                          parseRule)
import           Parser.SpeechParts.Atomics.Adjectives   (Adjective)
import           Parser.SpeechParts.Atomics.Adverbs      (ResearchAdverb (ResearchAdverb),
                                                          researchAdverbs)
import           Parser.SpeechParts.Atomics.Misc         (Determiner,
                                                          Partition (Partition),
                                                          partitions)
import           Parser.SpeechParts.Atomics.Nouns        (NamedAgent (NamedAgent),
                                                          namedAgents)
import           Parser.SpeechParts.Atomics.Prepositions (DirectionalStimulusMarker (..),
                                                          ObjectInterrogativeMarker (..),
                                                          SourceMarker (SourceMarker),
                                                          TopicMarker (TopicMarker),
                                                          directionalStimulusMarkers,
                                                          objectInterrogativeMarker,
                                                          sourceMarker,
                                                          topicMarker)
import           Parser.SpeechParts.Atomics.Verbs        (AcquisitionVerb (AcquisitionVerb),
                                                          CardinalMovementVerb (CardinalMovementVerb),
                                                          Copula (Copula),
                                                          DirectionalStimulusVerb (..),
                                                          DirectionalVerb (DirectionalVerb),
                                                          ExplicitBoundaryVerb (ExplicitBoundaryVerb),
                                                          ExplicitStimulusVerb (..),
                                                          GeneralPlacementVerb (GeneralPlacementVerb),
                                                          ImplicitBoundaryVerb (..),
                                                          ImplicitRegionalStimulusVerb (..),
                                                          ImplicitStimulusVerb (ImplicitStimulusVerb),
                                                          InstrumentActionVerb (InstrumentActionVerb),
                                                          InstrumentalAccessVerb (InstrumentalAccessVerb),
                                                          InstrumentalPlacementVerb (InstrumentalPlacementVerb),
                                                          ModToggleVerb (ModToggleVerb),
                                                          RotationalVerb (RotationalVerb),
                                                          SimpleAccessVerb (SimpleAccessVerb),
                                                          SpaceTransitionalVerb (..),
                                                          TargetedStimulusVerb (TargetedStimulusVerb),
                                                          ToggleVerb (ToggleVerb),
                                                          TransferVerb (TransferVerb),
                                                          TraversalPathVerb (TraversalPathVerb),
                                                          TraversalVerb (..),
                                                          acquisitionVerbs,
                                                          cardinalMovementVerbs,
                                                          copula,
                                                          directionalStimulusVerbs,
                                                          directionalVerbs,
                                                          explicitBoundaryVerbs,
                                                          explicitStimulusVerbs,
                                                          generalPlacementVerbs,
                                                          implicitBoundaryVerbs,
                                                          implicitRegionalStimulusVerbs,
                                                          implicitStimulusVerbs,
                                                          instrumentActionVerbs,
                                                          instrumentalAccessVerbs,
                                                          instrumentalPlacementVerbs,
                                                          modToggleVerbs,
                                                          rotationalVerbs,
                                                          simpleAccessVerbs,
                                                          spaceTransitionalVerbs,
                                                          targetedStimulusVerbs,
                                                          toggleVerbs,
                                                          transferVerbs,
                                                          traversalPathVerbs,
                                                          traversalVerbs)
import           Parser.SpeechParts.Composites.Nouns     (ObjectPhrase,
                                                          SupportPhrase)
import           Parser.SpeechParts.Composites.Verbs     (AccessVerbPhrase,
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
                                                          acquisitionVerbPhraseRule,
                                                          generalPlacementVerbPhraseRule,
                                                          vocativeRule)
import qualified Parser.SpeechParts.Composites.Verbs     (accessVerbPhraseRules,
                                                          imperativeRules,
                                                          interrogativeRules,
                                                          stimulusVerbPhraseRules)
import           Text.Earley.Grammar                     (Grammar, Prod)


copulaRule :: Grammar r (Prod r Text Lexeme Copula)
copulaRule = parseRule copula Copula

cardinalMovementRule :: Grammar r (Prod r Text Lexeme CardinalMovementVerb)
cardinalMovementRule = parseRule cardinalMovementVerbs CardinalMovementVerb

spaceTransitionalVerbRule :: Grammar r (Prod r Text Lexeme SpaceTransitionalVerb)
spaceTransitionalVerbRule = parseRule spaceTransitionalVerbs SpaceTransitionalVerb

implicitBoundaryVerbRule :: Grammar r (Prod r Text Lexeme ImplicitBoundaryVerb)
implicitBoundaryVerbRule = parseRule implicitBoundaryVerbs ImplicitBoundaryVerb

explicitBoundaryVerbRule :: Grammar r (Prod r Text Lexeme ExplicitBoundaryVerb)
explicitBoundaryVerbRule = parseRule explicitBoundaryVerbs ExplicitBoundaryVerb

implicitRegionalStimulusVerbRule :: Grammar r (Prod r Text Lexeme ImplicitRegionalStimulusVerb)
implicitRegionalStimulusVerbRule = parseRule implicitRegionalStimulusVerbs ImplicitRegionalStimulusVerb

implicitStimulusVerbRule :: Grammar r (Prod r Text Lexeme ImplicitStimulusVerb)
implicitStimulusVerbRule = parseRule implicitStimulusVerbs ImplicitStimulusVerb

explicitStimulusVerbRule :: Grammar r (Prod r Text Lexeme ExplicitStimulusVerb)
explicitStimulusVerbRule = parseRule explicitStimulusVerbs ExplicitStimulusVerb

directionalStimulusVerbRule :: Grammar r (Prod r Text Lexeme DirectionalStimulusVerb)
directionalStimulusVerbRule = parseRule directionalStimulusVerbs DirectionalStimulusVerb

targetedStimulusVerbRule :: Grammar r (Prod r Text Lexeme TargetedStimulusVerb)
targetedStimulusVerbRule = parseRule targetedStimulusVerbs TargetedStimulusVerb

traversalVerbRule :: Grammar r (Prod r Text Lexeme TraversalVerb)
traversalVerbRule = parseRule traversalVerbs TraversalVerb

traversalPathRule :: Grammar r (Prod r Text Lexeme TraversalPathVerb)
traversalPathRule = parseRule traversalPathVerbs TraversalPathVerb

toggleVerbRule :: Grammar r (Prod r Text Lexeme ToggleVerb)
toggleVerbRule = parseRule toggleVerbs ToggleVerb

modToggleVerbRule :: Grammar r (Prod r Text Lexeme ModToggleVerb)
modToggleVerbRule = parseRule modToggleVerbs ModToggleVerb

simpleAccessVerbRule :: Grammar r (Prod r Text Lexeme SimpleAccessVerb)
simpleAccessVerbRule = parseRule simpleAccessVerbs SimpleAccessVerb

instrumentalAccessVerbRule :: Grammar r (Prod r Text Lexeme InstrumentalAccessVerb)
instrumentalAccessVerbRule = parseRule instrumentalAccessVerbs InstrumentalAccessVerb

rotationalVerbRule :: Grammar r (Prod r Text Lexeme RotationalVerb)
rotationalVerbRule = parseRule rotationalVerbs RotationalVerb

directionalVerbRule :: Grammar r (Prod r Text Lexeme DirectionalVerb)
directionalVerbRule = parseRule directionalVerbs DirectionalVerb

instrumentActionVerbRule :: Grammar r (Prod r Text Lexeme InstrumentActionVerb)
instrumentActionVerbRule = parseRule instrumentActionVerbs InstrumentActionVerb

instrumentalPlacementVerbRule :: Grammar r (Prod r Text Lexeme InstrumentalPlacementVerb)
instrumentalPlacementVerbRule = parseRule instrumentalPlacementVerbs InstrumentalPlacementVerb

generalPlacementVerbRule :: Grammar r (Prod r Text Lexeme GeneralPlacementVerb)
generalPlacementVerbRule = parseRule generalPlacementVerbs GeneralPlacementVerb

acquisitionVerbRule :: Grammar r (Prod r Text Lexeme AcquisitionVerb)
acquisitionVerbRule = parseRule acquisitionVerbs AcquisitionVerb

transferVerbRule :: Grammar r (Prod r Text Lexeme TransferVerb)
transferVerbRule = parseRule transferVerbs TransferVerb

researchVerbRule :: Grammar r (Prod r Text Lexeme ResearchAdverb)
researchVerbRule = parseRule researchAdverbs ResearchAdverb


imperativeRules :: NounRules r
                            -> Prod r Text Lexeme Determiner
                            -> Prod r Text Lexeme Adjective
                            -> Grammar r (Prod r Text Lexeme Imperative)
imperativeRules nounParsers determiner adj = do
  _accessVerbPhrase <- accessVerbPhraseRules determiner adj
  _acquisitionVerbPhrase <- acquisitionVerbPhraseParser objectPhrase supportPhrase
  _generalPlacementVerbPhrase <- generalPlacementVerbPhraseParser objectPhrase supportPhrase
  _stimulusVerbPhrase <- stimulusVerbPhraseRules nounParsers determiner adj
  _cardinalMovementVerb <- cardinalMovementRule
  _implicitPath <- implicitPathRule
  Parser.SpeechParts.Composites.Verbs.imperativeRules $ ImperativeRules {..}
  where
    objectPhrase = nounParsers._objectPhrase'
    supportPhrase = nounParsers._supportPhrase'

interrogativeRules :: NounRules r
                            -> Grammar r (Prod r Text Lexeme Interrogative)
interrogativeRules nounParsers = do
  objectInterrogativeMarker' <- parseRule objectInterrogativeMarker ObjectInterrogativeMarker
  topicMarker' <- parseRule topicMarker TopicMarker
  locationInterrogativeMarker' <- _locationInterrogativeMarker' prepParsers
  copula' <- parseRule copula Copula
  Parser.SpeechParts.Composites.Verbs.interrogativeRules $ InterrogativeRules
    objectInterrogativeMarker'
    topicMarker'
    locationInterrogativeMarker'
    copula'
    objectPhrase
  where
    objectPhrase = nounParsers._objectPhrase'
    prepParsers = prepParser

vocativeParser :: Prod r Text Lexeme Imperative
                    -> NounRules r
                    -> Grammar r (Prod r Text Lexeme Vocative)
vocativeParser imperative nounParsers' = do
  namedAgent <- parseRule namedAgents NamedAgent -- does not belong in parser model
  partition <- parseRule partitions Partition
  interrogative <- interrogativeRules nounParsers'
  vocativeRule $ VocativeRules namedAgent partition imperative interrogative

accessVerbPhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme AccessVerbPhrase)
accessVerbPhraseRules determiner adj = do
  toggleVerb <- parseRule toggleVerbs ToggleVerb
  toggleNounPhrase <- toggleNounPhraseParser determiner adj
  modToggleVerb' <- parseRule modToggleVerbs ModToggleVerb
  modToggleNounPhrase <- modToggleNounPhraseParser determiner adj
  simpleAccessVerb <- parseRule simpleAccessVerbs SimpleAccessVerb
  simpleAccessNounPhrase <- simpleAccessNounPhraseParser determiner adj
  let accessVerbPhraseRules' = AccessVerbPhraseRules
                               toggleVerb
                               toggleNounPhrase
                               modToggleVerb'
                               modToggleNounPhrase
                               simpleAccessVerb
                               simpleAccessNounPhrase
  Parser.SpeechParts.Composites.Verbs.accessVerbPhraseRules accessVerbPhraseRules'

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

stimulusVerbPhraseRules :: NounRules r
                           -> Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseRules nounParsers determiner adj = do
  directionalStimulusNoun <- directionalStimulusNounParser determiner adj
  implicitStimulusVerb <- parseRule implicitStimulusVerbs ImplicitStimulusVerb
  explicitStimulusVerb <- parseRule explicitStimulusVerbs ExplicitStimulusVerb
  directionalStimulusVerb <- parseRule directionalStimulusVerbs DirectionalStimulusVerb
  directionalStimulusMarker <- parseRule directionalStimulusMarkers DirectionalStimulusMarker
  implicitRegionalStimulusVerb <- parseRule implicitRegionalStimulusVerbs ImplicitRegionalStimulusVerb
  targetedStimulusVerb <- parseRule targetedStimulusVerbs TargetedStimulusVerb
  Parser.SpeechParts.Composites.Verbs.stimulusVerbPhraseRules
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
