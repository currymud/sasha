module Build.BedPuzzle.Actions.Objects.Floor.Look
    (notEvenFloorF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration, updateActionConsequence)
import           Model.Core        (DirectionalStimulusActionF (CannotSeeF),
                                         GameComputation)

notEvenFloorF :: DirectionalStimulusActionF
notEvenFloorF = CannotSeeF notEvenFloor
  where
    notEvenFloor :: GameComputation Identity ()
    notEvenFloor = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One step at a time champ. You had a rough night. Open your eyes first."
