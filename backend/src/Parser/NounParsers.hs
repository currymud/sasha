{-# LANGUAGE RecordWildCards #-}
module Parser.NounParsers where
import           Data.Text                                (Text)
import           Lexer.Model                              (Lexeme)
import           Parser.Model.Nouns                       (NounParsers (..))
import           Parser.Model.Prepositions                (PrepParsers (..))
import           Parser.PrepParser                        (prepParser,
                                                           surfaceMarkerRule)
import           Parser.SpeechParts                       (parseRule)
import           Parser.SpeechParts.Atomics.Adverbs       (ModToggleAdverb (ModToggleAdverb),
                                                           modToggleAdverbs)
import           Parser.SpeechParts.Atomics.Misc          (Determiner)
import           Parser.SpeechParts.Atomics.Nouns         (Container (Container),
                                                           DirectionalStimulus (DirectionalStimulus),
                                                           ModToggleNoun (ModToggleNoun),
                                                           ObjectPath (..),
                                                           Objective (Objective),
                                                           SimpleAccessNoun (SimpleAccessNoun),
                                                           Surface (Surface),
                                                           TargetedStimulus (TargetedStimulus),
                                                           ToggleNoun (ToggleNoun),
                                                           containers,
                                                           directionalStimulii,
                                                           modToggleNouns,
                                                           objectPaths,
                                                           objectives,
                                                           simpleAccessNouns,
                                                           surfaces,
                                                           targetedStimulii,
                                                           toggleNouns)
import           Parser.SpeechParts.Atomics.Prepositions  (ContainmentMarker (ContainmentMarker),
                                                           SurfaceMarker,
                                                           TargetedStimulusMarker (..),
                                                           containmentMarkers)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase)
import           Parser.SpeechParts.Composites.Nouns      (ContainerPhrase,
                                                           ContainerPhraseRules (ContainerPhraseRules),
                                                           DirectionalStimulusNounPhrase,
                                                           DirectionalStimulusNounRules (DirectionalStimulusNounRules),
                                                           ModToggleNounPhrase,
                                                           ModToggleNounPhraseRules (ModToggleNounPhraseRules),
                                                           ObjectPathPhrase,
                                                           ObjectPathPhraseRules (ObjectPathPhraseRules),
                                                           ObjectPhrase,
                                                           ObjectPhraseRules (ObjectPhraseRules),
                                                           SimpleAccessNounPhrase,
                                                           SimpleAccessNounPhraseRules (SimpleAccessNounPhraseRules),
                                                           SupportPhrase,
                                                           SupportPhraseRules (SupportPhraseRules),
                                                           SurfacePhrase,
                                                           SurfacePhraseRules (SurfacePhraseRules),
                                                           TargetedStimulusNounPhrase,
                                                           TargetedStimulusNounPhraseRules (TargetedStimulusNounPhraseRules),
                                                           ToggleNounPhrase,
                                                           ToggleNounPhraseRules (ToggleNounPhraseRules),
                                                           containerPhraseRule,
                                                           directionalStimulusNounPhraseRule,
                                                           modToggleNounPhraseRule,
                                                           objectPathPhraseRule,
                                                           objectPhraseRule,
                                                           simpleAccessNounPhraseRule,
                                                           supportPhraseRule,
                                                           surfacePhraseRule,
                                                           targetedStimulusNounPhraseRule,
                                                           toggleNounPhraseRule)
import           Text.Earley.Grammar                      (Grammar, Prod)

nounParsers :: Prod r Text Lexeme Determiner
                 -> Prod r Text Lexeme AdjPhrase
                 -> Grammar r (NounParsers r)
nounParsers determiner adjPhrase = do
  targetedStimulusMarker' :: Prod r Text Lexeme TargetedStimulusMarker <- _targetedStimulusMarker' prepParser
  _containerPhrase' <- containerPhraseParser determiner adjPhrase
  _objectPhrase' <- objectivePhraseParser determiner adjPhrase
  _targetedStimulusNounPhrase'
    <- targetedStimulusNounPhraseParser
        determiner
        adjPhrase
        targetedStimulusMarker'
  _supportPhrase' <- supportPhraseParser determiner adjPhrase _containerPhrase'
  pure $ NounParsers { .. }
  where
    prepParsers = prepParser
    nounParsers' = nounParsers

objectivePhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme ObjectPhrase)
objectivePhraseParser determiner adjPhrase = do
  object <- parseRule objectives Objective
  let objectPhraseRules = ObjectPhraseRules determiner object adjPhrase
  objectPhraseRule objectPhraseRules

containerRule :: Grammar r (Prod r Text Lexeme Container)
containerRule = parseRule containers Container

containerPhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseParser determiner adjPhrase = do
  containmentMarker' <- parseRule containmentMarkers ContainmentMarker
  container <- containerRule
  let containerPhraseRules = ContainerPhraseRules
                               determiner
                               adjPhrase
                               container
                               containmentMarker'
  containerPhraseRule containerPhraseRules

targetedStimulusNounPhraseParser :: Prod r Text Lexeme Determiner
                      -> Prod r Text Lexeme AdjPhrase
                      -> Prod r Text Lexeme TargetedStimulusMarker
                      -> Grammar r (Prod r Text Lexeme TargetedStimulusNounPhrase)
targetedStimulusNounPhraseParser determiner adjPhrase targetedStimulusMarker' = do
  targetedStimulus <- parseRule targetedStimulii TargetedStimulus
  let targetStimulusRules = TargetedStimulusNounPhraseRules
                              targetedStimulusMarker'
                              targetedStimulus
                              determiner
                              adjPhrase
  targetedStimulusNounPhraseRule targetStimulusRules

directionalStimulusNounRule :: Grammar r (Prod r Text Lexeme DirectionalStimulus)
directionalStimulusNounRule = parseRule directionalStimulii DirectionalStimulus

directionalStimulusNounParser :: Prod r Text Lexeme Determiner
                                   -> Prod r Text Lexeme AdjPhrase
                                   -> Grammar r (Prod r Text Lexeme DirectionalStimulusNounPhrase)
directionalStimulusNounParser determiner adjPhrase = do
  directionalStimulus <- parseRule directionalStimulii DirectionalStimulus
  let directionalStimulusNounRules = DirectionalStimulusNounRules determiner adjPhrase directionalStimulus
  directionalStimulusNounPhraseRule directionalStimulusNounRules

toggleNounRule :: Grammar r (Prod r Text Lexeme ToggleNoun)
toggleNounRule = parseRule toggleNouns ToggleNoun

toggleNounPhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme ToggleNounPhrase)
toggleNounPhraseParser determiner adjPhrase = do
  toggleNoun <- toggleNounRule
  let toggleNounPhraseRules = ToggleNounPhraseRules
                              determiner
                              adjPhrase
                              toggleNoun
  toggleNounPhraseRule toggleNounPhraseRules


modToggleNounRule :: Grammar r (Prod r Text Lexeme ModToggleNoun)
modToggleNounRule = parseRule modToggleNouns ModToggleNoun

modToggleNounPhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme ModToggleNounPhrase)
modToggleNounPhraseParser determiner adjPhrase = do
  modToggleAdverbs' <- parseRule modToggleAdverbs ModToggleAdverb
  modToggleNoun <- modToggleNounRule
  let modToggleNounPhraseRules = ModToggleNounPhraseRules
                                  determiner
                                  adjPhrase
                                  modToggleNoun
                                  modToggleAdverbs'
  modToggleNounPhraseRule modToggleNounPhraseRules

objectPathParser :: Grammar r (Prod r Text Lexeme ObjectPath)
objectPathParser = parseRule objectPaths ObjectPath

objectPathPhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme ObjectPath
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme ObjectPathPhrase)
objectPathPhraseParser determiner objectPath adjPhrase =
  objectPathPhraseRule objectPathPhraseRules
  where
  objectPathPhraseRules = ObjectPathPhraseRules objectPath determiner adjPhrase

simpleAccessNounPhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Grammar r (Prod r Text Lexeme SimpleAccessNounPhrase)
simpleAccessNounPhraseParser determiner adjPhrase  = do
  simpleAccessNoun <- parseRule simpleAccessNouns SimpleAccessNoun
  let simpleAccessNounPhraseRules = SimpleAccessNounPhraseRules
                                  determiner
                                  adjPhrase
                                  simpleAccessNoun
  simpleAccessNounPhraseRule simpleAccessNounPhraseRules

surfacePhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Prod r Text Lexeme Surface
                           -> Prod r Text Lexeme SurfaceMarker
                           -> Grammar r (Prod r Text Lexeme SurfacePhrase)
surfacePhraseParser determiner adjPhrase surface surfaceMarker =
  surfacePhraseRule surfacePhraseRules
  where
    surfacePhraseRules = SurfacePhraseRules determiner adjPhrase surface surfaceMarker

surfaceRule :: Grammar r (Prod r Text Lexeme Surface)
surfaceRule = parseRule surfaces Surface

supportPhraseParser :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme AdjPhrase
                           -> Prod r Text Lexeme ContainerPhrase
                           -> Grammar r (Prod r Text Lexeme SupportPhrase)
supportPhraseParser determiner adjPhrase containerPhrase = do
  surfaceRule' <- surfaceRule
  surfaceMarkerRule' <- surfaceMarkerRule
  let surfacePhraseRules = SurfacePhraseRules
                            determiner
                            adjPhrase
                            surfaceRule'
                            surfaceMarkerRule'
  surfacePhraseRule' <- surfacePhraseRule surfacePhraseRules
  let supportPhraseRules = SupportPhraseRules
                               surfacePhraseRule'
                               containerPhrase
  supportPhraseRule supportPhraseRules
