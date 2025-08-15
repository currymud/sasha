{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Floor.Get (getFloorDeniedF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (AcquisitionActionF (CollectedF),
                                         GameComputation,
                                         updateActionConsequence)
getFloorDeniedF :: AcquisitionActionF
getFloorDeniedF = CollectedF denied
  where
    denied :: Either (GameComputation Identity ()) (GameComputation Identity ())
    denied = Left $ modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try to pick up the floor. It doesn't budge. Physics wins again."
