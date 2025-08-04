{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Locations.Bedroom.Get (get,getDenied) where
import           Control.Monad.Identity        (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (getObjectM,
                                                getPlayerLocationGID,
                                                modifyLocationM,
                                                modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF),
                                                ActionEffectMap,
                                                GameComputation,
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase)
import           Model.Parser.GCase            (NounKey)


getDenied :: AcquisitionActionF
getDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "not possibe"

get :: AcquisitionActionF
get = AcquisitionActionF getit
  where
    getit :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity ()
    getit loc actionEffectMap avp = do
      let (objectPhrase, nounKey) = parseAcquisitionPhrase avp

      -- Find the object in the current location
      case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
        Just objSet | not (Data.Set.null objSet) -> do
          let oid = Data.Set.elemAt 0 objSet  -- Taking first object (no disambiguation)

          -- Remove the object from the location's semantic map
          let updatedObjectSemanticMap = Data.Map.Strict.adjust (Data.Set.delete oid) nounKey loc._objectSemanticMap
              -- If the set becomes empty, remove the key entirely
              finalObjectSemanticMap = if Data.Set.null (Data.Map.Strict.findWithDefault Data.Set.empty nounKey updatedObjectSemanticMap)
                                      then Data.Map.Strict.delete nounKey updatedObjectSemanticMap
                                      else updatedObjectSemanticMap

          -- Update the location in the game state
          playerLocationGID <- getPlayerLocationGID
          modifyLocationM playerLocationGID $ \loc' -> loc' { _objectSemanticMap = finalObjectSemanticMap }

          -- Add success message
          obj <- getObjectM oid
          modifyNarration $ updateActionConsequence ("You take the " <> _description obj)

        _ -> modifyNarration $ updateActionConsequence "You don't see that here."
