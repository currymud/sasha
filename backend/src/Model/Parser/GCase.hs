{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Parser.GCase where
import           Data.Kind    (Type)
import           GHC.Generics (Generic)

type VerbKeys :: Type
data VerbKeys
  = CardinalMovementKey
  | ImplicitBoundaryKey
  | ImplicitStimulusKey
  | DirectionalStimulusKey
  | SimpleAccessKey
  | ImplicitRegionalStimulusKey
  deriving stock (Show, Eq, Ord, Generic)
