{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Pill.Get (getPill,getPillDenied,alreadyHavePill) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (getObjectM, modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description),
                                                Player (_inventory),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase            (NounKey)

alreadyHavePill :: AcquisitionActionF
alreadyHavePill = AcquisitionActionF (const (const (const havePill)))
  where
    havePill :: GameComputation Identity ()
    havePill = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already have the pill in your inventory."

getPillDenied :: AcquisitionActionF
getPillDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

getPill :: AcquisitionActionF
getPill = AcquiredFromF getit
  where
    getit :: Location -> AcquisitionVerbPhrase -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
    getit loc avp = do
      let (objectPhrase, nounKey) = parseAcquisitionPhrase avp

      -- Find the object in the current location
      case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
        Just objSet | not (Data.Set.null objSet) -> do
          let oid = Data.Set.elemAt 0 objSet  -- Taking first object (no disambiguation)

          -- Add the object to player's inventory
          modify' $ \gs ->
            let player = gs._player
                currentInventory = player._inventory
                updatedInventory = Data.Map.Strict.insertWith Data.Set.union objectPhrase (Data.Set.singleton oid) currentInventory
                updatedPlayer = player { _inventory = updatedInventory }
            in gs { _player = updatedPlayer }

          -- Add success message
          obj <- getObjectM oid
          pure $ Right $ modifyNarration $ updateActionConsequence ("You take the " <> _description obj <> " and put it in your inventory.")

        _ -> pure $ Left $ modifyNarration $ updateActionConsequence "You don't see that here."
