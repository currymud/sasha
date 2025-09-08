module ConstraintRefinement.Actions.Objects.Pill.Take (alreadyTookPillF,
                                                  pillTooFarF,
                                                  takePillF,
                                                  takePillDeniedF) where

import           Control.Monad.Identity        (Identity)
import           Data.Set                      (Set)
import           Data.Text                     (Text)
import           GameState                     (modifyNarration,
                                                updateActionConsequence)
import           Model.Core                    (ActionEffectKey (PlayerKey),
                                                ActionEffectMap,
                                                ConsumptionActionF (ConsumptionActionF),
                                                GameComputation, Object)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

takePillDeniedF :: ConsumptionActionF
takePillDeniedF = ConsumptionActionF (const (const (const (const denied))))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't take the pill right now."


alreadyTookPillF :: ConsumptionActionF
alreadyTookPillF = ConsumptionActionF (const (const (const (const denied))))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already took the pill."

pillTooFarF :: ConsumptionActionF
pillTooFarF = ConsumptionActionF (const (const (const (const denied))))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You grab at it but it's hard to get to. try grabbing the robe first."

takePillF :: ConsumptionActionF
takePillF = ConsumptionActionF takePill
  where
    takePill :: GID Object -> Set ActionEffectKey -> ActionEffectMap -> ConsumptionVerbPhrase -> GameComputation Identity ()
    takePill _targetOid _actionKeys _effectMap _cvp = do
      -- Add success narration
      modifyNarration
        $ updateActionConsequence "You take the pill and immediately feel better. Your headache is gone and you feel ready to get up."
