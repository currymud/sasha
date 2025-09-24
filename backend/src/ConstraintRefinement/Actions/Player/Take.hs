module ConstraintRefinement.Actions.Player.Take (takeDenied) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             ConsumptionActionF (CannotConsumeF),
                                             GameComputation)

takeDenied :: ConsumptionActionF
takeDenied = CannotConsumeF denied
  where
    denied :: ActionEffectKey ->  GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey
