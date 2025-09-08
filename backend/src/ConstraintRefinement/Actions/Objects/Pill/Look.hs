module ConstraintRefinement.Actions.Objects.Pill.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (DirectionalStimulusActionF (CannotSeeF),
                                         GameComputation (GameComputation),
                                         Location)

whatPill :: DirectionalStimulusActionF
whatPill = CannotSeeF whatPill'
  where
    whatPill' ::GameComputation Identity ()
    whatPill'  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "You don't see a pill here, but you have a hazy memory of having one in your bathrobe pocket"

notEvenPill :: DirectionalStimulusActionF
notEvenPill = CannotSeeF notEvenPill'
  where
    notEvenPill' :: GameComputation Identity ()
    notEvenPill' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "It's a slow start to the day, your eyes aren't even open yet, let alone seeing a pill."
