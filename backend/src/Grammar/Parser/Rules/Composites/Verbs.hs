module Grammar.Parser.Rules.Composites.Verbs
  (stimulusVerbPhraseRules
  , imperativeRules
  ) where
import           Data.Text                                               (Text)
import           GHC.Base                                                (Alternative ((<|>)))
import           Grammar.Parser.Lexer                                    (Lexeme)
import           Grammar.Parser.Partitions.Adjectives                    (adjectives)
import           Grammar.Parser.Partitions.Misc                          (determiners)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (directionalStimulii)
import           Grammar.Parser.Partitions.Nouns.Edibles                 (edibles)
import           Grammar.Parser.Partitions.Nouns.SomaticStimulus         (somaticStimulii)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (acquisitionVerbs)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (directionalStimulusVerbs)
import           Grammar.Parser.Partitions.Verbs.EdibleConsumptionVerbs  (edibleConsumptionVerbs)
import           Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (somaticAccessVerbs)
import           Grammar.Parser.Rules.Atomics.Prepositions               (directionalStimulusMarkerRule)
import           Grammar.Parser.Rules.Atomics.Utils                      (parseRule)
import           Grammar.Parser.Rules.Atomics.Verbs                      (implicitStimulusVerbRule)
import           Grammar.Parser.Rules.Composites.Nouns                   (directionalStimulusNounPhraseRules,
                                                                          edibleNounPhraseRules,
                                                                          somaticStimulusNounPhraseRules)
import           Model.Parser.Atomics.Adjectives                         (Adjective (Adjective))
import           Model.Parser.Atomics.Misc                               (Determiner (Determiner))
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus (DirectionalStimulus),
                                                                          Edible (Edible),
                                                                          SomaticStimulus (SomaticStimulus))
import           Model.Parser.Atomics.Verbs                              (AcquisitionVerb (AcquisitionVerb),
                                                                          DirectionalStimulusVerb (DirectionalStimulusVerb),
                                                                          EdibleConsumptionVerb (EdibleConsumptionVerb),
                                                                          SomaticAccessVerb (SomaticAccessVerb))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase,
                                                                          ConsumptionVerbPhrase (EdibleVerbPhrase),
                                                                          Imperative (ConsumptionVerbPhrase, StimulusVerbPhrase),
                                                                          StimulusVerbPhrase (DirectStimulusVerbPhrase, ImplicitStimulusVerb, SomaticStimulusVerbPhrase))
import           Text.Earley.Grammar                                     (Grammar,
                                                                          Prod,
                                                                          rule)

stimulusVerbPhraseRules :: Grammar r (Prod r Text Lexeme StimulusVerbPhrase)
stimulusVerbPhraseRules = do
  implicitStimulusVerb <- implicitStimulusVerbRule
  directionalStimulusMarker <- directionalStimulusMarkerRule
  directionalStimulusVerb <- parseRule directionalStimulusVerbs DirectionalStimulusVerb
  directionalStimulusNoun <- parseRule directionalStimulii DirectionalStimulus
  somaticAccessVerb <- parseRule somaticAccessVerbs SomaticAccessVerb
  somaticStimulus <- parseRule somaticStimulii SomaticStimulus
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective
  directionalStimulusNounPhrase <- directionalStimulusNounPhraseRules determiner adj directionalStimulusNoun
  somaticStimulusNounPhrase <- somaticStimulusNounPhraseRules determiner adj somaticStimulus
  rule $ ImplicitStimulusVerb <$> implicitStimulusVerb
           <|> DirectStimulusVerbPhrase
             <$> directionalStimulusVerb
             <*> directionalStimulusMarker
             <*> directionalStimulusNounPhrase
           <|> SomaticStimulusVerbPhrase
             <$> somaticAccessVerb
             <*> somaticStimulusNounPhrase
{-
acquisitionVerbPhraseRules :: Grammar r (Prod r Text Lexeme AcquisitionVerbPhrase)
acquisitionVerbPhraseRules = do
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective
  acquisitionVerb <- parseRule acquisitionVerbs AcquisitionVerb
  objectPhrase <- objectPhrase
  rule $ SomaticVerbPhrase <$> somaticAccessVerb
           <*> somaticStimulusNounPhrase
-}
consumptionVerbPhraseRules :: Grammar r (Prod r Text Lexeme ConsumptionVerbPhrase)
consumptionVerbPhraseRules = do
  determiner <- parseRule determiners Determiner
  adj <- parseRule adjectives Adjective
  edible <- parseRule edibles Edible
  edibleNounPhrase <- edibleNounPhraseRules determiner adj edible
  edibleConsumptionVerb <- parseRule edibleConsumptionVerbs EdibleConsumptionVerb
  rule $ EdibleVerbPhrase <$> edibleConsumptionVerb
           <*> edibleNounPhrase

imperativeRules :: Grammar r (Prod r Text Lexeme Imperative)
imperativeRules = do
  stimulusVerbPhrase <- stimulusVerbPhraseRules
  consumptionVerbPhrase <- consumptionVerbPhraseRules
  rule $ StimulusVerbPhrase <$> stimulusVerbPhrase
           <|> ConsumptionVerbPhrase <$> consumptionVerbPhrase
