{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Model.Parser.GCase where
import           Data.Kind                     (Type)
import           GHC.Generics                  (Generic)
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Nouns    (Consumable, Container,
                                                DirectionalStimulus, Objective,
                                                SimpleAccessNoun, Supportive,
                                                ToggleNoun)
import           Model.Parser.Atomics.Verbs    (CardinalMovementVerb,
                                                ConsumptionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitBoundaryVerb,
                                                ImplicitRegionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SimpleAccessVerb)
import           Model.Parser.Composites.Verbs (Imperative (PosturalVerbPhrase, StimulusVerbPhrase),
                                                PosturalVerbPhrase (NegativePosturalVerbPhrase, PositivePosturalVerbPhrase),
                                                StimulusVerbPhrase (..))

type VerbKey :: Type
data VerbKey
  = CardinalMovementKey         CardinalMovementVerb
  | DirectionalStimulusVerbKey  DirectionalStimulusVerb
  | ConsumptionKey              ConsumptionVerb
  | ImplicitBoundaryKey         ImplicitBoundaryVerb
  | ImplicitStimulusKey         ImplicitStimulusVerb
  | SimpleAccessVerbKey         SimpleAccessVerb
  | ImplicitRegionalStimulusKey ImplicitRegionalStimulusVerb
  | PositivePosturalVerbKey     PositivePosturalVerb
  | NegativePosturalVerbKey     NegativePosturalVerb
  deriving stock (Show, Eq, Ord, Generic)

mkVerbKey :: Sentence -> VerbKey
mkVerbKey (Imperative imperative) = case imperative of
  (StimulusVerbPhrase vphrase) -> fromStimulusVerbPhrase vphrase
  (PosturalVerbPhrase vphrase) -> fromPosturalVerbPhrase vphrase

fromStimulusVerbPhrase :: StimulusVerbPhrase -> VerbKey
fromStimulusVerbPhrase (ImplicitStimulusVerb v) = ImplicitStimulusKey v

fromPosturalVerbPhrase :: PosturalVerbPhrase -> VerbKey
fromPosturalVerbPhrase (PositivePosturalVerbPhrase verb _) = PositivePosturalVerbKey verb
fromPosturalVerbPhrase (NegativePosturalVerbPhrase verb _) = NegativePosturalVerbKey verb


type NounKey :: Type
data NounKey
  = DirectionalStimulusKey DirectionalStimulus
  | ObjectiveKey Objective
  | SupportiveKey Supportive
  | ContainerKey Container
  | ToggleNounKey ToggleNoun
  | ModToggleNounKey ToggleNoun
  | SimpleAccessNounKey SimpleAccessNoun
  | ConsumableNounKey Consumable
  deriving stock (Show, Eq, Ord, Generic)
