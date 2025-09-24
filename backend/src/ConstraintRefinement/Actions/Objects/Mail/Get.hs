{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Mail.Get (getMailDeniedF,alreadyHaveMailF,getMailDizzyF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (CollectedF, NotGettableF),
                                             ActionEffectKey, GameComputation)


alreadyHaveMailF :: AcquisitionActionF
alreadyHaveMailF = NotGettableF haveMail
  where
    haveMail :: ActionEffectKey -> GameComputation Identity ()
    haveMail actionEffectKey = processEffectsFromRegistry actionEffectKey

getMailDeniedF :: AcquisitionActionF
getMailDeniedF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey

getMailDizzyF :: AcquisitionActionF
getMailDizzyF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey
