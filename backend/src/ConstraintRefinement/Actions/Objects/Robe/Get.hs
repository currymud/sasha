module ConstraintRefinement.Actions.Objects.Robe.Get (getRobeDeniedF, alreadyHaveRobeF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (AcquisitionActionF (NotGettableF),
                                         GameComputation)

alreadyHaveRobeF :: AcquisitionActionF
alreadyHaveRobeF = NotGettableF haveRobe
  where
    haveRobe :: GameComputation Identity ()
    haveRobe = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You are already wearing the robe."

getRobeDeniedF :: AcquisitionActionF
getRobeDeniedF = NotGettableF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't see it. You're dizzy with a hangover from the night before. Open your eyes."
