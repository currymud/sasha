module Build.BedPuzzle.Effects.Perception.Look where
import           GameState.Perception (youSeeM)
import           Model.GameState      (SystemEffect (PerceptionSystemEffect))



youSeeMEffect :: SystemEffect
youSeeMEffect = PerceptionSystemEffect youSeeM
