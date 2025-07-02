{-# LANGUAGE RecordWildCards #-}
module Parser.NounRules where
import           Data.Text                               (Text)
import           Lexer.Model                             (Lexeme)
import           Parser.Model.Nouns                      (NounRules (..))
import           Parser.Model.Prepositions               (PrepRuless (..))
import           Parser.PrepRules                        (prepRules,
                                                          surfaceMarkerRule)
import           Parser.SpeechParts                      (parseRule)
import           Parser.SpeechParts.Atomics.Adjectives   (Adjective)
import           Parser.SpeechParts.Atomics.Adverbs      (ModToggleAdverb (ModToggleAdverb),
                                                          modToggleAdverbs)
import           Parser.SpeechParts.Atomics.Misc         (Determiner)
import           Parser.SpeechParts.Atomics.Nouns        (Container (Container),
                                                          DirectionalStimulus (DirectionalStimulus),
                                                          ModToggleNoun (ModToggleNoun),
                                                          NamedAgent (..),
                                                          ObjectPath (..),
                                                          Objective (Objective),
                                                          SimpleAccessNoun (SimpleAccessNoun),
                                                          Surface (Surface),
                                                          TargetedStimulus (TargetedStimulus),
                                                          ToggleNoun (ToggleNoun),
                                                          containers,
                                                          directionalStimulii,
                                                          modToggleNouns,
                                                          namedAgents,
                                                          objectPaths,
                                                          objectives,
                                                          simpleAccessNouns,
                                                          surfaces,
                                                          targetedStimulii,
                                                          toggleNouns)
import           Parser.SpeechParts.Atomics.Prepositions (ContainmentMarker (ContainmentMarker),
                                                          SurfaceMarker,
                                                          TargetedStimulusMarker (..),
                                                          containmentMarkers)
import           Parser.SpeechParts.Composites.Nouns     (ContainerPhrase,
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
                                                          ToggleNounPhraseRules (ToggleNounPhraseRules))
import qualified Parser.SpeechParts.Composites.Nouns     (containerPhraseRule,
                                                          directionalStimulusNounPhraseRule,
                                                          modToggleNounPhraseRule,
                                                          objectPathPhraseRule,
                                                          objectPhraseRule,
                                                          simpleAccessNounPhraseRule,
                                                          supportPhraseRule,
                                                          surfacePhraseRule,
                                                          targetedStimulusNounPhraseRule,
                                                          toggleNounPhraseRule)
import           Text.Earley.Grammar                     (Grammar, Prod)

nounRules :: Prod r Text Lexeme Determiner
                 -> Prod r Text Lexeme Adjective
                 -> Grammar r (NounRules r)
nounRules determiner adj = do
  targetedStimulusMarker' <- _targetedStimulusMarker' prepRules
  _containerPhrase' <- containerPhraseRules determiner adj
  _objectPhrase' <- objectivePhraseRules determiner adj
  _targetedStimulusNounPhrase'
    <- targetedStimulusNounPhraseRules
        determiner
        adj
        targetedStimulusMarker'
  _supportPhrase' <- supportPhraseRules determiner adj _containerPhrase'
  pure $ NounRules { .. }
  where
    prepRuless = prepRules
--    nounRuless' = nounRules

objectivePhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme ObjectPhrase)
objectivePhraseRules determiner adj = do
  object <- parseRule objectives Objective
  let objectPhraseRules = ObjectPhraseRules determiner object adj
  Rules.SpeechParts.Composites.Nouns.objectPhraseRule objectPhraseRules

containerRule :: Grammar r (Prod r Text Lexeme Container)
containerRule = parseRule containers Container

containerPhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme ContainerPhrase)
containerPhraseRules determiner adj = do
  containmentMarker' <- parseRule containmentMarkers ContainmentMarker
  container <- containerRule
  let containerPhraseRules = ContainerPhraseRules
                               determiner
                               adj
                               container
                               containmentMarker'
  containerPhraseRule containerPhraseRules

targetedStimulusRule :: Grammar r (Prod r Text Lexeme TargetedStimulus)
targetedStimulusRule = parseRule targetedStimulii TargetedStimulus

targetedStimulusNounPhraseRules :: Prod r Text Lexeme Determiner
                      -> Prod r Text Lexeme Adjective
                      -> Prod r Text Lexeme TargetedStimulusMarker
                      -> Grammar r (Prod r Text Lexeme TargetedStimulusNounPhrase)
targetedStimulusNounPhraseRules determiner adj targetedStimulusMarker' = do
  targetedStimulus <- targetedStimulusRule
  let targetStimulusRules = TargetedStimulusNounPhraseRules
                              targetedStimulusMarker'
                              targetedStimulus
                              determiner
                              adj
  targetedStimulusNounPhraseRule targetStimulusRules

directionalStimulusNounRule :: Grammar r (Prod r Text Lexeme DirectionalStimulus)
directionalStimulusNounRule = parseRule directionalStimulii DirectionalStimulus

directionalStimulusNounRules :: Prod r Text Lexeme Determiner
                                   -> Prod r Text Lexeme Adjective
                                   -> Grammar r (Prod r Text Lexeme DirectionalStimulusNounPhrase)
directionalStimulusNounRules determiner adj = do
  directionalStimulus <- directionalStimulusNounRule
  let directionalStimulusNounRules = DirectionalStimulusNounRules
                                       determiner
                                       adj
                                       directionalStimulus
  directionalStimulusNounPhraseRule directionalStimulusNounRules

toggleNounRule :: Grammar r (Prod r Text Lexeme ToggleNoun)
toggleNounRule = parseRule toggleNouns ToggleNoun

toggleNounPhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme ToggleNounPhrase)
toggleNounPhraseRules determiner adj = do
  toggleNoun <- toggleNounRule
  let toggleNounPhraseRules = ToggleNounPhraseRules
                              determiner
                              adj
                              toggleNoun
  toggleNounPhraseRule toggleNounPhraseRules

modToggleNounRule :: Grammar r (Prod r Text Lexeme ModToggleNoun)
modToggleNounRule = parseRule modToggleNouns ModToggleNoun

modToggleNounPhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme ModToggleNounPhrase)
modToggleNounPhraseRules determiner adj = do
  modToggleAdverbs' <- parseRule modToggleAdverbs ModToggleAdverb
  modToggleNoun <- modToggleNounRule
  let modToggleNounPhraseRules = ModToggleNounPhraseRules
                                  determiner
                                  adj
                                  modToggleNoun
                                  modToggleAdverbs'
  modToggleNounPhraseRule modToggleNounPhraseRules

namedAgentRule :: Grammar r (Prod r Text Lexeme NamedAgent)
namedAgentRule = parseRule namedAgents NamedAgent

objectPathRule :: Grammar r (Prod r Text Lexeme ObjectPath)
objectPathRule = parseRule objectPaths ObjectPath

objectPathPhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme ObjectPath
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme ObjectPathPhrase)
objectPathPhraseRules determiner objectPath adj =
  objectPathPhraseRule objectPathPhraseRules
  where
  objectPathPhraseRules = ObjectPathPhraseRules objectPath determiner adj

simpleAccessNounRule :: Grammar r (Prod r Text Lexeme SimpleAccessNoun)
simpleAccessNounRule = parseRule simpleAccessNouns SimpleAccessNoun

simpleAccessNounPhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Grammar r (Prod r Text Lexeme SimpleAccessNounPhrase)
simpleAccessNounPhraseRules determiner adj = do
  simpleAccessNoun <- simpleAccessNounRule
  let simpleAccessNounPhraseRules = SimpleAccessNounPhraseRules
                                  determiner
                                  adj
                                  simpleAccessNoun
  simpleAccessNounPhraseRule simpleAccessNounPhraseRules

surfacePhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Prod r Text Lexeme Surface
                           -> Prod r Text Lexeme SurfaceMarker
                           -> Grammar r (Prod r Text Lexeme SurfacePhrase)
surfacePhraseRules determiner adj surface surfaceMarker =
  surfacePhraseRule surfacePhraseRules
  where
    surfacePhraseRules = SurfacePhraseRules determiner adj surface surfaceMarker

surfaceRule :: Grammar r (Prod r Text Lexeme Surface)
surfaceRule = parseRule surfaces Surface

supportPhraseRules :: Prod r Text Lexeme Determiner
                           -> Prod r Text Lexeme Adjective
                           -> Prod r Text Lexeme ContainerPhrase
                           -> Grammar r (Prod r Text Lexeme SupportPhrase)
supportPhraseRules determiner adj containerPhrase = do
  surfaceRule' <- surfaceRule
  surfaceMarkerRule' <- surfaceMarkerRule
  let surfacePhraseRules = SurfacePhraseRules
                            determiner
                            adj
                            surfaceRule'
                            surfaceMarkerRule'
  surfacePhraseRule' <- surfacePhraseRule surfacePhraseRules
  let supportPhraseRules = SupportPhraseRules
                               surfacePhraseRule'
                               containerPhrase
  supportPhraseRule supportPhraseRules
