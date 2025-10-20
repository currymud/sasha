module ConstraintRefinement.Actions.Objects.Open where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ContainerAccessActionF (ObjectContainerAccessF),
                                            ObjectContainerAccessActionF (ContainingObjectCanAccessF, ContainingObjectCannotAccessF))


-- Both denial and success use same function, differeniated with effects
openContainerF :: ContainerAccessActionF
openContainerF = ObjectContainerAccessF processEffectsFromRegistry

-- New role-specific implementations (keeping old ones for build integrity)
openObjectContainerF :: ObjectContainerAccessActionF  
openObjectContainerF = ContainingObjectCanAccessF processEffectsFromRegistry

openObjectContainerDeniedF :: ObjectContainerAccessActionF
openObjectContainerDeniedF = ContainingObjectCannotAccessF processEffectsFromRegistry
