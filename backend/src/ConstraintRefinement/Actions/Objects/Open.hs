module ConstraintRefinement.Actions.Objects.Open where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ContainerAccessActionF (ObjectContainerAccessF))


-- Both denial and success use same function, differeniated with effects
openContainerF :: ContainerAccessActionF
openContainerF = ObjectContainerAccessF processEffectsFromRegistry
