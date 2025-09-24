module ConstraintRefinement.Actions.Objects.Pocket.Look (whatPocket,notEvenPocket,pocketClosedF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             DirectionalStimulusActionF (CannotSeeF),
                                             DirectionalStimulusContainerActionF (CannotSeeInF),
                                             GameComputation)

whatPocket :: DirectionalStimulusActionF
whatPocket = CannotSeeF whatPocket'
  where
    whatPocket' :: ActionEffectKey -> GameComputation Identity ()
    whatPocket' actionEffectKey = processEffectsFromRegistry actionEffectKey

notEvenPocket :: DirectionalStimulusActionF
notEvenPocket = CannotSeeF notEvenPocket'
  where
    notEvenPocket' :: ActionEffectKey -> GameComputation Identity ()
    notEvenPocket' actionEffectKey = processEffectsFromRegistry actionEffectKey

pocketClosedF :: DirectionalStimulusContainerActionF
pocketClosedF = CannotSeeInF pocketClosed'
  where
    pocketClosed' :: ActionEffectKey -> GameComputation Identity ()
    pocketClosed' actionEffectKey = processEffectsFromRegistry actionEffectKey
