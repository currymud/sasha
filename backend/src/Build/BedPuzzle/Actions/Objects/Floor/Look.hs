module Build.BedPuzzle.Actions.Objects.Floor.Look
    (notEvenFloorF,seeFloorF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation,
                                         updateActionConsequence)

seeFloorF :: DirectionalStimulusActionF
seeFloorF = DirectionalStimulusActionF (const (const seeFloor'))
  where
    seeFloor' :: GameComputation Identity ()
    seeFloor' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It's a wooden floor, with a few cracks and splinters. " <>
          "It looks like it hasn't been cleaned in a while, and there are some dust bunnies in the corners."

notEvenFloorF :: DirectionalStimulusActionF
notEvenFloorF = DirectionalStimulusActionF (const (const notEvenFloor'))
  where
    notEvenFloor' :: GameComputation Identity ()
    notEvenFloor' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One step at a time champ. You had a rough night. Open your eyes first."
