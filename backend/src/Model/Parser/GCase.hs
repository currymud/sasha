{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser.GCase where
import           Data.Kind                  (Type)
import           GHC.Generics               (Generic)
import           Model.Parser.Atomics.Verbs (CardinalMovementVerb,
                                             DirectionalStimulusVerb,
                                             ImplicitBoundaryVerb,
                                             ImplicitRegionalStimulusVerb,
                                             ImplicitStimulusVerb,
                                             SimpleAccessVerb)

type VerbKeys :: Type
data VerbKeys
  = CardinalMovementKey CardinalMovementVerb
  | ImplicitBoundaryKey ImplicitBoundaryVerb
  | ImplicitStimulusKey ImplicitStimulusVerb
  | DirectionalStimulusKey DirectionalStimulusVerb
  | SimpleAccessKey SimpleAccessVerb
  | ImplicitRegionalStimulusKey ImplicitRegionalStimulusVerb
  deriving stock (Show, Eq, Ord, Generic)
