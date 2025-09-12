module ConstraintRefinement.Effects.Perception.Look where
import qualified Data.Set
import           GameState.ActionManagement (processEffects)
import           GameState.Perception (youSeeM)
import           Model.Core           (SystemEffect (PerceptionSystemEffect))

youSeeMEffect :: SystemEffect
youSeeMEffect = PerceptionSystemEffect youSeeM
