{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Model.Parser.GCase where
import           Data.Kind                     (Type)
import           GHC.Generics                  (Generic)
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
  | DirectionalStimulusKey      DirectionalStimulusVerb
  | SimpleAccessKey             SimpleAccessVerb
  | ImplicitRegionalStimulusKey ImplicitRegionalStimulusVerb
  deriving stock (Show, Eq, Ord, Generic)

mkVerbKey :: Imperative -> VerbKey
mkVerbKey (StimulusVerbPhrase vphrase) = fromStimulusVerbPhrase vphrase

fromStimulusVerbPhrase :: StimulusVerbPhrase -> VerbKey
fromStimulusVerbPhrase (ImplicitStimulusVerb v) = ImplicitStimulusKey v
