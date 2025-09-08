{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Pill.Get (getPillDeniedF,alreadyHavePillF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (AcquisitionActionF (NotGettableF),
                                         GameComputation)

alreadyHavePillF :: AcquisitionActionF
alreadyHavePillF = NotGettableF havePill
  where
    havePill :: (GameComputation Identity ())
    havePill = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already have the pill in your inventory."

getPillDeniedF :: AcquisitionActionF
getPillDeniedF = NotGettableF denied
  where
    denied :: (GameComputation Identity ())
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"
