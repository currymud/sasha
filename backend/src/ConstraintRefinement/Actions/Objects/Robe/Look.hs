module ConstraintRefinement.Actions.Objects.Robe.Look (notEvenRobeF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

notEvenRobeF :: DirectionalStimulusActionF
notEvenRobeF = CannotSeeF notEvenPill'
  where
    notEvenPill' :: ActionEffectKey -> GameComputation Identity ()
    notEvenPill' actionEffectKey = processEffectsFromRegistry actionEffectKey
