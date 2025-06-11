module Parser.PhraseParsers where
import           Data.Text                                (Text)
import           Lexer.Model                              (Lexeme)
import           Parser.NounParsers                       (modToggleNounPhraseParser,
                                                           simpleAccessNounPhraseParser,
                                                           toggleNounPhraseParser)
import           Parser.SpeechParts                       (parseRule)
import           Parser.SpeechParts.Atomics.Adjectives    (Adjective (Adjective),
                                                           adjectives)
import           Parser.SpeechParts.Atomics.Adverbs       (ModToggleAdverb (ModToggleAdverb),
                                                           modToggleAdverbs)
import           Parser.SpeechParts.Atomics.Misc          (Determiner)
import           Parser.SpeechParts.Atomics.Nouns         (Container (Container),
                                                           DirectionalStimulus (DirectionalStimulus),
                                                           ModToggleNoun (ModToggleNoun),
                                                           Objective (Objective),
                                                           SimpleAccessNoun (SimpleAccessNoun),
                                                           Surface (Surface),
                                                           TargetedStimulus (TargetedStimulus),
                                                           ToggleNoun (ToggleNoun),
                                                           containers,
                                                           directionalStimulii,
                                                           modToggleNouns,
                                                           objectives,
                                                           simpleAccessNouns,
                                                           surfaces,
                                                           targetedStimulii,
                                                           toggleNouns)
import           Parser.SpeechParts.Atomics.Prepositions  (ContainmentMarker (ContainmentMarker),
                                                           SurfaceMarker (SurfaceMarker),
                                                           TargetedStimulusMarker,
                                                           containmentMarkers,
                                                           surfaceMarkers)
import           Parser.SpeechParts.Atomics.Verbs         (ModToggleVerb (ModToggleVerb),
                                                           SimpleAccessVerb (SimpleAccessVerb),
                                                           ToggleVerb (ToggleVerb),
                                                           modToggleVerbs,
                                                           simpleAccessVerbs,
                                                           toggleVerbs)
import           Parser.SpeechParts.Composites.Adjectives (AdjPhrase,
                                                           AdjPhraseRules (AdjPhraseRules),
                                                           adjPhraseRule)
import           Parser.SpeechParts.Composites.Nouns      (ContainerPhrase,
                                                           ContainerPhraseRules (ContainerPhraseRules),
                                                           DirectionalStimulusNoun,
                                                           DirectionalStimulusNounRules (DirectionalStimulusNounRules),
                                                           ModToggleNounPhrase,
                                                           ModToggleNounPhraseRules (ModToggleNounPhraseRules),
                                                           ObjectPhrase,
                                                           ObjectPhraseRules (ObjectPhraseRules),
                                                           SimpleAccessNounPhrase,
                                                           SimpleAccessNounPhraseRules (SimpleAccessNounPhraseRules),
                                                           SupportPhrase,
                                                           SupportPhraseRules (SupportPhraseRules),
                                                           SurfacePhraseRules (SurfacePhraseRules),
                                                           TargetedStimulusNounPhrase,
                                                           TargetedStimulusNounPhraseRules (TargetedStimulusNounPhraseRules),
                                                           ToggleNounPhrase,
                                                           ToggleNounPhraseRules (ToggleNounPhraseRules),
                                                           containerPhraseRule,
                                                           directionalStimulusNounRule,
                                                           modToggleNounPhraseRule,
                                                           objectPhraseRule,
                                                           simpleAccessNounPhraseRule,
                                                           supportPhraseRule,
                                                           surfacePhraseRule,
                                                           targetedStimulusNounPhraseRule,
                                                           toggleNounPhraseRule)
import           Parser.SpeechParts.Composites.Verbs      (AccessVerbPhrase,
                                                           AccessVerbPhraseRules (AccessVerbPhraseRules),
                                                           accessVerbPhraseRule)
import           Text.Earley.Grammar                      (Grammar, Prod)

adjectivePhraseParser :: Grammar r (Prod r Text Lexeme AdjPhrase)
adjectivePhraseParser = mdo
  adj <- parseRule adjectives Adjective
  adjSecondary <- parseRule adjectives Adjective
  let adjPhraseRules = AdjPhraseRules adj adjSecondary
  adjPhraseRule adjPhraseRules
