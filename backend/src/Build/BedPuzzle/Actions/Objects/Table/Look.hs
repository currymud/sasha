module Build.BedPuzzle.Actions.Objects.Table.Look where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation (GameComputation),
                                         Location, updateActionConsequence)

whatTable :: DirectionalStimulusActionF
whatTable = DirectionalStimulusActionF (const (const (const whatTable')))
  where
    whatTable' :: GameComputation Identity ()
    whatTable' = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "It would be alot easier to see the table if you would open your eyes. Literally."

seeTable :: DirectionalStimulusActionF
seeTable = DirectionalStimulusActionF (const (const (const seeTable')))
  where
    seeTable' :: GameComputation Identity ()
    seeTable' = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You see a table. It is a table. It is not a very interesting table, but it is a table nonetheless."
