module ConstraintRefinement.Actions.Objects.Robe.Get (getRobeDeniedF, alreadyHaveRobeF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey, GameComputation)

alreadyHaveRobeF :: AcquisitionActionF
alreadyHaveRobeF = NotGettableF haveRobe
  where
    haveRobe :: ActionEffectKey -> GameComputation Identity ()
    haveRobe actionEffectKey = processEffectsFromRegistry actionEffectKey
                                 >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You are already wearing the robe."

getRobeDeniedF :: AcquisitionActionF
getRobeDeniedF = NotGettableF denied
  where
    denied :: ActionEffectKey ->  GameComputation Identity ()
    denied actionEffectKey  = processEffectsFromRegistry actionEffectKey
                               >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You can't see it. You're dizzy with a hangover from the night before. Open your eyes."
