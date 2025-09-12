module ConstraintRefinement.Actions.Player.Take (takeDenied) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (ConsumptionActionF (CannotConsumeF),
                                         GameComputation)
import           Prelude                hiding (take)

takeDenied :: ConsumptionActionF
takeDenied = CannotConsumeF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

