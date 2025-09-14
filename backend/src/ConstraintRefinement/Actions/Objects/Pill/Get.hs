{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Pill.Get (getPillDeniedF,alreadyHavePillF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey, GameComputation)

alreadyHavePillF :: AcquisitionActionF
alreadyHavePillF = NotGettableF havePill
  where
    havePill :: ActionEffectKey -> GameComputation Identity ()
    havePill actionEffectKey = processEffectsFromRegistry actionEffectKey
                                 >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You already have the pill in your inventory."

getPillDeniedF :: AcquisitionActionF
getPillDeniedF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey  = processEffectsFromRegistry actionEffectKey
                                >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"
