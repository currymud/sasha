{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Model.Parser.GCase where
import           Data.Kind                     (Type)
import           GHC.Generics                  (Generic)
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Nouns    (Container, DirectionalStimulus,
                                                Objective, SimpleAccessNoun,
                                                Supportive, ToggleNoun)
import           Model.Parser.Atomics.Verbs    (CardinalMovementVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitBoundaryVerb,
                                                ImplicitRegionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                SimpleAccessVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (..))

type VerbKey :: Type
data VerbKey
  = CardinalMovementKey         CardinalMovementVerb
  | ImplicitBoundaryKey         ImplicitBoundaryVerb
  | ImplicitStimulusKey         ImplicitStimulusVerb
  | DirectionalStimulusVerbKey      DirectionalStimulusVerb
  | SimpleAccessVerbKey             SimpleAccessVerb
  | ImplicitRegionalStimulusKey ImplicitRegionalStimulusVerb
  deriving stock (Show, Eq, Ord, Generic)

mkVerbKey :: Sentence -> VerbKey
mkVerbKey (Imperative imperative) = case imperative of
  (StimulusVerbPhrase vphrase) -> fromStimulusVerbPhrase vphrase


fromStimulusVerbPhrase :: StimulusVerbPhrase -> VerbKey
fromStimulusVerbPhrase (ImplicitStimulusVerb v) = ImplicitStimulusKey v

type NounKey :: Type
data NounKey
  = DirectionalStimulusKey DirectionalStimulus
  | ObjectiveKey Objective
  | SupportiveKey Supportive
  | ContainerKey Container
  | ToggleNounKey ToggleNoun
  | ModToggleNounKey ToggleNoun
  | SimpleAccessNounKey SimpleAccessNoun
  deriving stock (Show, Eq, Ord, Generic)
