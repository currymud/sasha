module ConstraintRefinement.Actions.Objects.Chair.Look (whatChairF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)

whatChairF :: DirectionalStimulusActionF
whatChairF = CannotSeeF whatChair'
  where
    whatChair' :: ActionEffectKey -> GameComputation Identity ()
    whatChair' actionEffectKey = processEffectsFromRegistry actionEffectKey
