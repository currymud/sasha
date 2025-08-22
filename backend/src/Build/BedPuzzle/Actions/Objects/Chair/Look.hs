module Build.BedPuzzle.Actions.Objects.Chair.Look (whatChairF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation,
                                         updateActionConsequence)

whatChairF :: DirectionalStimulusActionF
whatChairF = DirectionalStimulusActionF (const (const whatChair'))
  where
    whatChair' :: GameComputation Identity ()
    whatChair' = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "It would be alot easier to see the chair if you would open your eyes. Literally."
