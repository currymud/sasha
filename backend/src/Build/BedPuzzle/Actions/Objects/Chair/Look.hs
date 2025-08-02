module Build.BedPuzzle.Actions.Objects.Chair.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation,
                                         updateActionConsequence)

whatChair :: DirectionalStimulusActionF
whatChair = DirectionalStimulusActionF (const (const (const whatChair')))
  where
    whatChair' :: GameComputation Identity ()
    whatChair' = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "It would be alot easier to see the chair if you would open your eyes. Literally."

seeChair :: DirectionalStimulusActionF
seeChair = DirectionalStimulusActionF (const (const (const seeChair')))
  where
    seeChair' :: GameComputation Identity ()
    seeChair' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You see a chair. It is a chair. It is not a very interesting chair, but it is a chair nonetheless."
