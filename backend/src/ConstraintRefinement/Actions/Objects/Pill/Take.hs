module ConstraintRefinement.Actions.Objects.Pill.Take (alreadyTookPillF,
                                                  pillTooFarF,
                                                  takePillF,
                                                  takePillDeniedF) where

import           Control.Monad.Identity        (Identity)
import           Data.Set                      (Set)
import           Data.Text                     (Text)
import           GameState                     (modifyNarration,
                                                updateActionConsequence)
import           Model.Core                    (ConsumptionActionF (CannotConsumeF, PlayerConsumptionActionF),
                                                GameComputation, Object,
                                                TargetEffectKey (PlayerKey),
                                                TargetEffectMap)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

takePillDeniedF :: ConsumptionActionF
takePillDeniedF = CannotConsumeF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't take the pill right now."


alreadyTookPillF :: ConsumptionActionF
alreadyTookPillF = CannotConsumeF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already took the pill."

pillTooFarF :: ConsumptionActionF
pillTooFarF = CannotConsumeF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You grab at it but it's hard to get to. try grabbing the robe first."

takePillF :: ConsumptionActionF
takePillF = PlayerConsumptionActionF takePill
  where
    takePill :: GID Object -> Set TargetEffectKey -> TargetEffectMap -> ConsumptionVerbPhrase -> GameComputation Identity ()
    takePill _targetOid _actionKeys _effectMap _cvp = do
      -- Add success narration
      modifyNarration
        $ updateActionConsequence "You take the pill and immediately feel better. Your headache is gone and you feel ready to get up."
