{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Robe.Get (getRobe,getRobeDenied,alreadyHaveRobe) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (addToInventoryM, getObjectM,
                                                modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF),
                                                GameComputation (GameComputation),
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase            (NounKey)
import           Relude.String                 (ToText (toText))

alreadyHaveRobe :: AcquisitionActionF
alreadyHaveRobe = AcquisitionActionF (const (const (const haveRobe)))
  where
    haveRobe :: GameComputation Identity ()
    haveRobe = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You are already wearing the robe."

getRobeDenied :: AcquisitionActionF
getRobeDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't see it. You're dizzy with a hangover from the night before. Open your eyes."

getRobe :: AcquisitionActionF
getRobe = AcquiredFromF getit
  where
    getit :: Location -> AcquisitionVerbPhrase -> GameComputation Identity ( Either (GameComputation Identity ()) (GameComputation Identity ()) )
    getit loc avp = do
      let (objectPhrase, nounKey) = parseAcquisitionPhrase avp

      -- Find the object in the current location
      case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
        Just objSet | not (Data.Set.null objSet) -> do
          let oid = Data.Set.elemAt 0 objSet  -- Taking first object (no disambiguation)
          addToInventoryM oid
          pure $ Right $ modifyNarration $ updateActionConsequence ("You take the " <> toText objectPhrase <> " and put it in your inventory.")

        _ -> pure $ Left $ modifyNarration $ updateActionConsequence "You don't see that here."
