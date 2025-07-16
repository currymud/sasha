{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.Action.LocationAction where
import           Data.Kind (Type)
import           Data.Text (Text)

type PlayerActionF :: Type -> Type
data PlayerActionF a
  = ImplicitStimulusF -- Placeholder for player action logic



