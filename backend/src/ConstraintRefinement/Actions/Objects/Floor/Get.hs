{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Floor.Get (getFloorDeniedF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey, GameComputation)

getFloorDeniedF :: AcquisitionActionF
getFloorDeniedF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey
