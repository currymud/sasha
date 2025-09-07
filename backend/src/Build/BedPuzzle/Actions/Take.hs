{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Build.BedPuzzle.Actions.Take (takeDenied) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration, updateActionConsequence)
import           Model.Core        (ConsumptionActionF (ConsumptionActionF),
                                         GameComputation)
import           Prelude                hiding (take)

takeDenied :: ConsumptionActionF
takeDenied = ConsumptionActionF (const (const (const (const denied))))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

