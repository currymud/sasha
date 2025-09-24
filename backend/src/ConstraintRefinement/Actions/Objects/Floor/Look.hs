module ConstraintRefinement.Actions.Objects.Floor.Look
    (notEvenFloorF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

notEvenFloorF :: DirectionalStimulusActionF
notEvenFloorF = CannotSeeF notEvenFloor
  where
    notEvenFloor :: ActionEffectKey -> GameComputation Identity ()
    notEvenFloor actionEffectKey = processEffectsFromRegistry actionEffectKey
