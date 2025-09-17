{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Floor.Get (getFloorDeniedF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey, GameComputation)

getFloorDeniedF :: AcquisitionActionF
getFloorDeniedF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to pick up the floor. It doesn't budge. Physics wins again."
