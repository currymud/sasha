module ConstraintRefinement.Actions.Player.Take (takeDenied) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             ConsumptionActionF (CannotConsumeF),
                                             GameComputation)

takeDenied :: ConsumptionActionF
takeDenied = CannotConsumeF denied
  where
    denied :: ActionEffectKey ->  GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
      modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"
