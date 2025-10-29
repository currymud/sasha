module ConstraintRefinement.Actions.Objects.Open where
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ObjectContainerAccessActionF (ContainingObjectCanAccessF, ContainingObjectCannotAccessF))

-- New role-specific implementations (keeping old ones for build integrity)
openObjectContainerF :: ObjectContainerAccessActionF
openObjectContainerF = ContainingObjectCanAccessF processEffectsFromRegistry

openObjectContainerDeniedF :: ObjectContainerAccessActionF
openObjectContainerDeniedF = ContainingObjectCannotAccessF processEffectsFromRegistry
