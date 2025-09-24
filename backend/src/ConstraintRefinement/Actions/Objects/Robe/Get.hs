module ConstraintRefinement.Actions.Objects.Robe.Get (getRobeDeniedF, alreadyHaveRobeF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey, GameComputation)

alreadyHaveRobeF :: AcquisitionActionF
alreadyHaveRobeF = NotGettableF haveRobe
  where
    haveRobe :: ActionEffectKey -> GameComputation Identity ()
    haveRobe actionEffectKey = processEffectsFromRegistry actionEffectKey

getRobeDeniedF :: AcquisitionActionF
getRobeDeniedF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey
