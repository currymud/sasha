module ConstraintRefinement.Actions.Objects.Pocket.Look (whatPocket,notEvenPocket,pocketClosedF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (DirectionalStimulusActionF (CannotSeeF),
                                         DirectionalStimulusContainerActionF (CannotSeeInF),
                                         GameComputation)

whatPocket :: DirectionalStimulusActionF
whatPocket = CannotSeeF whatPocket'
  where
    whatPocket' ::GameComputation Identity ()
    whatPocket'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "Pocket? What Pocket?"

notEvenPocket :: DirectionalStimulusActionF
notEvenPocket = CannotSeeF notEvenPocket'
  where
    notEvenPocket' :: GameComputation Identity ()
    notEvenPocket' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "One thing at a time. You've just woken up and your eyes are all bleary unfocused and closed. Maybe open them up and go from there?"

pocketClosedF :: DirectionalStimulusContainerActionF
pocketClosedF = CannotSeeInF pocketClosed'
  where
    pocketClosed' :: GameComputation Identity ()
    pocketClosed' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "The pocket is velcroed shut."
