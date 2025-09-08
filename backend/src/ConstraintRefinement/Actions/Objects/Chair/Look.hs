module ConstraintRefinement.Actions.Objects.Chair.Look (whatChairF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (DirectionalStimulusActionF (CannotSeeF),
                                         GameComputation)

whatChairF :: DirectionalStimulusActionF
whatChairF = CannotSeeF whatChair'
  where
    whatChair' :: GameComputation Identity ()
    whatChair' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It would be alot easier to see the chair if you would open your eyes. Literally."
