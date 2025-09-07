module Build.BedPuzzle.Effects.Perception.Look where
import           GameState.Perception (youSeeM)
import           Model.Core      (SystemEffect (PerceptionSystemEffect))



youSeeMEffect :: SystemEffect
youSeeMEffect = PerceptionSystemEffect youSeeM
