module ConstraintRefinement.Actions.Objects.Pill.Take (alreadyTookPillF,
                                                  pillTooFarF,
                                                  takePillF,
                                                  takePillDeniedF) where

import           Control.Monad.Identity        (Identity)
import           Data.Set                      (Set)
import           Data.Text                     (Text)
import           GameState                     (modifyNarration,
                                                updateActionConsequence)
import           GameState.ActionManagement    (processEffectsFromRegistry)
import           Model.Core                    (ActionEffectKey,
                                                ActionEffectMap,
                                                ConsumptionActionF (CannotConsumeF, PlayerConsumptionActionF),
                                                GameComputation, Object,
                                                TargetEffectKey)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

takePillDeniedF :: ConsumptionActionF
takePillDeniedF = CannotConsumeF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't take the pill right now."

alreadyTookPillF :: ConsumptionActionF
alreadyTookPillF = CannotConsumeF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey  = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already took the pill."

pillTooFarF :: ConsumptionActionF
pillTooFarF = CannotConsumeF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You grab at it but it's hard to get to. try grabbing the robe first."

takePillF :: ConsumptionActionF
takePillF = PlayerConsumptionActionF takePill
  where
    takePill :: ActionEffectKey -> GID Object -> ConsumptionVerbPhrase -> GameComputation Identity ()
    takePill actionEffectKey _oid _cvp = do
    --  ToDo
      -- Add success narration
      modifyNarration
        $ updateActionConsequence "You take the pill and immediately feel better. Your headache is gone and you feel ready to get up."
      processEffectsFromRegistry actionEffectKey
