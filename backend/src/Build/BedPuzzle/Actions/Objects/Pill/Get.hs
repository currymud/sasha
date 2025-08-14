{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Pill.Get (getPill,getPillDenied,alreadyHavePill) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (addToInventoryM,
                                                modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase            (NounKey)
import           Relude.String                 (ToText (toText))

alreadyHavePill :: AcquisitionActionF
alreadyHavePill = AcquiredFromF (Left havePill)
  where
    havePill :: GameComputation Identity ()
    havePill = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already have the pill in your inventory."

getPillDenied :: AcquisitionActionF
getPillDenied = AcquiredFromF (Left denied)
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

getPill :: AcquisitionActionF
getPill = AcquiredFromF getit
  where
    getit :: (Either (GameComputation Identity ()) (GameComputation Identity ()))
    getit = Left (pure ())
