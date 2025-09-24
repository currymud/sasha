module ConstraintRefinement.Actions.Player.Stand (standDenied, standUp,standUpDenied) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey, GameComputation,
                                             PosturalActionF (CannotPosturalActionF, PlayerPosturalActionF))

standDenied :: PosturalActionF
standDenied = CannotPosturalActionF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey

standUp :: PosturalActionF
standUp = PlayerPosturalActionF stood
  where
    stood :: ActionEffectKey -> GameComputation Identity ()
    stood actionEffectKeys = processEffectsFromRegistry actionEffectKeys

standUpDenied :: PosturalActionF
standUpDenied = CannotPosturalActionF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey
