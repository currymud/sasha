module Build.BedPuzzle.Actions.Objects.Pocket.Open (notEvenOpenF, pocketOutOfReachF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (ContainerAccessActionF (CannotAccessF),
                                         GameComputation,
                                         updateActionConsequence)

notEvenOpenF :: ContainerAccessActionF
notEvenOpenF = CannotAccessF notEvenOpen
  where
    notEvenOpen :: GameComputation Identity ()
    notEvenOpen  = modifyNarration $ updateActionConsequence msg

    msg :: Text
    msg = "This is rather difficult as your eyes are closed"

pocketOutOfReachF :: ContainerAccessActionF
pocketOutOfReachF = CannotAccessF outOfReach
  where
    outOfReach :: GameComputation Identity ()
    outOfReach = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You'll need to get the robe first"
