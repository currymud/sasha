module Grammar.Parser.Rules.Composites.Verbs
  (stimulusVerbPhraseRules
  , acquisitionVerbPhraseRules
  , containerVerbPhraseRules
  , imperativeRules
  , consumptionVerbPhraseRules
  , posturalVerbPhraseRules
  ) where
import           Data.Text                                                        (Text)
import           GHC.Base                                                         (Alternative ((<|>)))
import           Grammar.Parser.Lexer                                             (Lexeme)
import           Grammar.Parser.Partitions.Adjectives                             (adjectives)
import           Grammar.Parser.Partitions.Misc                                   (determiners)
import           Grammar.Parser.Partitions.Nouns.Consumables                      (consumables)
import           Grammar.Parser.Partitions.Nouns.Containers                       (containers)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus              (directionalStimulii)
import           Grammar.Parser.Partitions.Nouns.Instruments                      (instruments)
import           Grammar.Parser.Partitions.Nouns.Objectives                       (objectives)
import           Grammar.Parser.Partitions.Nouns.SomaticStimulus                  (somaticStimulii)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (containmentMarker)
import           Grammar.Parser.Partitions.Prepositions.SourceMarkers             (sourceMarkers)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs                 (acquisitionVerbs)
import           Grammar.Parser.Partitions.Verbs.AdministrativeVerbs              (administrativeVerbs)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs                 (consumptionVerbs)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb          (directionalStimulusVerbs)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs               (somaticAccessVerbs)
import           Grammar.Parser.Rules.Atomics.Adverbs                             (negativePosturalDirectionRule,
                                                                                   positivePosturalDirectionRule)
import           Grammar.Parser.Rules.Atomics.Prepositions                        (directionalStimulusMarkerRule,
                                                                                   instrumentMarkerRule)
import           Grammar.Parser.Rules.Atomics.Utils                               (parseRule)
import           Grammar.Parser.Rules.Atomics.Verbs                               (implicitStimulusVerbRule,
                                                                                   negativePosturalVerbRule,
                                                                                   positivePosturalVerbRule,
                                                                                   simpleAccessVerbRule)
import           Grammar.Parser.Rules.Composites.Nouns                            (consumableNounPhraseRules,
                                                                                   containerPhraseRules,
                                                                                   directionalStimulusContainerPhraseRules,
                                                                                   directionalStimulusNounPhraseRules,
                                                                                   instrumentalAccessNounPhraseRules,
                                                                                   objectPhraseRules,
                                                                                   somaticStimulusNounPhraseRules,
                                                                                   supportPhraseRules)
import           Model.Parser.Atomics.Adjectives                                  (Adjective (Adjective))
import           Model.Parser.Atomics.Misc                                        (Determiner (Determiner))
import           Model.Parser.Atomics.Nouns                                       (Consumable (Consumable),
                                                                                   Container (Container),
                                                                                   DirectionalStimulus (DirectionalStimulus),
                                                                                   InstrumentalAccessNoun (InstrumentalAccessNoun),
                                                                                   Objective (Objective),
                                                                                   SomaticStimulus (SomaticStimulus))
import           Model.Parser.Atomics.Prepositions                                (ContainmentMarker (ContainmentMarker),
                                                                                   SourceMarker (SourceMarker))
import           Model.Parser.Atomics.Verbs                                       (AcquisitionVerb (AcquisitionVerb),
                                                                                   AdministrativeVerb (AdministrativeVerb),
                                                                                   ConsumptionVerb (ConsumptionVerb),
                                                                                   DirectionalStimulusVerb (DirectionalStimulusVerb),
                                                                                   SomaticAccessVerb (SomaticAccessVerb))
import           Model.Parser.Composites.Verbs                                    (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase),
                                                                                   ConsumptionVerbPhrase (ConsumptionVerbPhrase),
                                                                                   ContainerAccessVerbPhrase (ContainerAccessVerbPhrase, SimpleAccessContainerVerbPhrase),
                                                                                   Imperative (AcquisitionVerbPhrase', Administrative, ConsumptionVerbPhrase', ContainerAccessVerbPhrase', PosturalVerbPhrase, StimulusVerbPhrase),
                                                                                   PosturalVerbPhrase (NegativePosturalVerbPhrase, PositivePosturalVerbPhrase),
                                                                                   StimulusVerbPhrase (DirectStimulusVerbPhrase, DirectionalStimulusContainmentPhrase, ImplicitStimulusVerb, SomaticStimulusVerbPhrase))
import           Text.Earley.Grammar                                              (Grammar,
                                                                                   Prod,
                                                                                   rule)
                                                                            {-
directionalStimulusContainerPhraseRules :: Prod r Text Lexeme ContainmentMarker
                                       -> Prod r Text Lexeme ContainerPhrase
                                       -> Grammar r (Prod r Text Lexeme DirectionalStimulusContainerPhrase)
-}
stimulusVerbPhraseRules :: Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseRules = do
  implicitStimulusVerb <- implicitStimulusVerbRule
  directionalStimulusMarker <- directionalStimulusMarkerRule
  directionalStimulusVerb <- parseRule directionalStimulusVerbs DirectionalStimulusVerb
  directionalStimulusNoun <- parseRule directionalStimulii DirectionalStimulus
  somaticAccessVerb <- parseRule somaticAccessVerbs SomaticAccessVerb
  somaticStimulus <- parseRule somaticStimulii SomaticStimulus
  determiner <- parseRule determiners Determiner
  container <- parseRule containers Container
  adj <- parseRule adjectives Adjective
  containerPhrase <- containerPhraseRules determiner adj container
  containmentMarker' <- parseRule containmentMarker ContainmentMarker
  dsContainerPhrase <- directionalStimulusContainerPhraseRules containmentMarker' containerPhrase
  directionalStimulusNounPhrase <- directionalStimulusNounPhraseRules determiner adj directionalStimulusMarker directionalStimulusNoun
  somaticStimulusNounPhrase <- somaticStimulusNounPhraseRules determiner adj somaticStimulus
  rule $ ImplicitStimulusVerb <$> implicitStimulusVerb
           <|> DirectStimulusVerbPhrase
             <$> directionalStimulusVerb
             <*> directionalStimulusNounPhrase
           <|> DirectionalStimulusContainmentPhrase
                 <$> directionalStimulusVerb
                 <*> dsContainerPhrase
           <|> SomaticStimulusVerbPhrase
             <$> somaticAccessVerb
             <*> somaticStimulusNounPhrase

containerVerbPhraseRules :: Grammar r (Prod r Text Lexeme ContainerAccessVerbPhrase)
containerVerbPhraseRules = do
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective
  container <- parseRule containers Container
  simpleAccessVerb <- simpleAccessVerbRule
  containerPhrase <- containerPhraseRules determiner adj container
  instrumentMarker <- instrumentMarkerRule
  instrumentDeterminer <- parseRule determiners Determiner
  instrumentAdj <- parseRule adjectives Adjective
  instumentAccessNoun <- parseRule instruments InstrumentalAccessNoun
  instrumentNounPhrase <- instrumentalAccessNounPhraseRules instrumentDeterminer instrumentAdj instrumentMarker instumentAccessNoun
  rule $ SimpleAccessContainerVerbPhrase
           <$> simpleAccessVerb
           <*> containerPhrase
         <|> ContainerAccessVerbPhrase
           <$> simpleAccessVerb
           <*> containerPhrase
           <*> instrumentNounPhrase


acquisitionVerbPhraseRules :: Grammar r (Prod r Text Lexeme AcquisitionVerbPhrase)
acquisitionVerbPhraseRules = do
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective
  sourceMarker <- parseRule sourceMarkers SourceMarker
  acquisitionVerb <- parseRule acquisitionVerbs AcquisitionVerb
  object <- parseRule objectives Objective
  objectPhrase <- objectPhraseRules determiner adj object
  supportPhrase <- supportPhraseRules determiner adj
  rule $ SimpleAcquisitionVerbPhrase
           <$> acquisitionVerb
           <*> objectPhrase
         <|> AcquisitionVerbPhrase
           <$> acquisitionVerb
           <*> objectPhrase
           <*> sourceMarker
           <*> supportPhrase

consumptionVerbPhraseRules :: Grammar r (Prod r Text Lexeme ConsumptionVerbPhrase)
consumptionVerbPhraseRules = do
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective
  consumable <- parseRule consumables Consumable
  consumableNounPhrase <- consumableNounPhraseRules determiner adj consumable
  consumptionVerb <- parseRule consumptionVerbs ConsumptionVerb
  rule $ ConsumptionVerbPhrase <$> consumptionVerb
           <*> consumableNounPhrase

posturalVerbPhraseRules :: Grammar r (Prod r Text Lexeme PosturalVerbPhrase)
posturalVerbPhraseRules = do
  positivePosturalVerb <- positivePosturalVerbRule
  negativePosturalVerb <- negativePosturalVerbRule
  positivePosturalDirection <- positivePosturalDirectionRule
  negativePosturalDirection <- negativePosturalDirectionRule
  rule $ PositivePosturalVerbPhrase <$> positivePosturalVerb <*> positivePosturalDirection
      <|> NegativePosturalVerbPhrase <$> negativePosturalVerb <*> negativePosturalDirection

imperativeRules :: Grammar r (Prod r Text Lexeme Imperative)
imperativeRules = do
  administrativeVerb <- parseRule administrativeVerbs AdministrativeVerb
  containerVerbPhrase <- containerVerbPhraseRules
  stimulusVerbPhrase <- stimulusVerbPhraseRules
  consumptionVerbPhrase <- consumptionVerbPhraseRules
  acquisitionVerbPhrase <- acquisitionVerbPhraseRules
  posturalVerbPhrase <- posturalVerbPhraseRules
  rule $ Administrative <$> administrativeVerb
           <|> ContainerAccessVerbPhrase' <$> containerVerbPhrase
           <|> StimulusVerbPhrase <$> stimulusVerbPhrase
           <|> ConsumptionVerbPhrase' <$> consumptionVerbPhrase
           <|> AcquisitionVerbPhrase' <$> acquisitionVerbPhrase
           <|> PosturalVerbPhrase <$> posturalVerbPhrase
