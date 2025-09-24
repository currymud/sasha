module ConstraintRefinement.Actions.Objects.Pocket.Open (notEvenOpenF, pocketOutOfReachF) where
import           Control.Monad.Identity     (Identity)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey,
                                             ContainerAccessActionF (CannotAccessF),
                                             GameComputation)

notEvenOpenF :: ContainerAccessActionF
notEvenOpenF = CannotAccessF notEvenOpen
  where
    notEvenOpen :: ActionEffectKey -> GameComputation Identity ()
    notEvenOpen actionEffectKey = processEffectsFromRegistry actionEffectKey

pocketOutOfReachF :: ContainerAccessActionF
pocketOutOfReachF = CannotAccessF outOfReach
  where
    outOfReach :: ActionEffectKey -> GameComputation Identity ()
    outOfReach actionEffectKey = processEffectsFromRegistry actionEffectKey
