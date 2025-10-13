module ConstraintRefinement.Actions.Player.Take (takeDenied) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             ConsumptionActionF (PlayerCannotConsumeF),
                                             GameComputation)

takeDenied :: ConsumptionActionF
takeDenied = PlayerCannotConsumeF denied
  where
    denied :: ActionEffectKey ->  GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey
