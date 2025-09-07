{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Floor.Get (getFloorDeniedF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration, updateActionConsequence)
import           Model.Core        (AcquisitionActionF (NotGettableF),
                                         GameComputation)
getFloorDeniedF :: AcquisitionActionF
getFloorDeniedF = NotGettableF denied
  where
    denied :: (GameComputation Identity ())
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to pick up the floor. It doesn't budge. Physics wins again."
