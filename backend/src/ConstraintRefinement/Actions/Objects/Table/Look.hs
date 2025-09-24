module ConstraintRefinement.Actions.Objects.Table.Look where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             GameComputation)
whatTable :: DirectionalStimulusActionF
whatTable = CannotSeeF whatTable'
  where
    whatTable' :: ActionEffectKey -> GameComputation Identity ()
    whatTable' actionEffectKey = processEffectsFromRegistry actionEffectKey

seeTable :: DirectionalStimulusActionF
seeTable = CannotSeeF seeTable'
  where
    seeTable' :: ActionEffectKey -> GameComputation Identity ()
    seeTable' actiolEffectKey = processEffectsFromRegistry actiolEffectKey
