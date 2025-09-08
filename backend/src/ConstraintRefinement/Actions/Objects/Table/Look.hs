module ConstraintRefinement.Actions.Objects.Table.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (DirectionalStimulusActionF (CannotSeeF),
                                         GameComputation (GameComputation),
                                         Location)
-- ToDo naming convention fix
whatTable :: DirectionalStimulusActionF
whatTable = CannotSeeF whatTable'
  where
    whatTable' :: GameComputation Identity ()
    whatTable' = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "It would be alot easier to see the table if you would open your eyes. Literally."

seeTable :: DirectionalStimulusActionF
seeTable = CannotSeeF seeTable'
  where
    seeTable' :: GameComputation Identity ()
    seeTable' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You see a table. It is a table. It is not a very interesting table, but it is a table nonetheless."
