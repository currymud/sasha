{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import           Control.Monad.State           (gets)
import qualified Data.Map.Strict
import           Data.Set                      (Set, elemAt, null, toList)
import           GameState                     (getPlayerLocationM, getPlayerM)
import           GameState.ActionManagement    (lookupAcquisition,
                                                processEffectsFromRegistry)
import           GameState.EffectRegistry      (lookupEffectsInRegistry)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                GameComputation,
                                                GameState (_world),
                                                Location (_objectSemanticMap),
                                                Object,
                                                Player (_location, _playerActions),
                                                SearchStrategy,
                                                SpatialRelationship (ContainedIn, SupportedBy),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (SimpleNounPhrase))
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase),
                                                StimulusVerbPhrase (DirectStimulusVerbPhrase))
import           Model.Parser.GCase            (NounKey (DirectionalStimulusKey))

manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  pure ()

-- | General case: Search current location's object semantic map
locationSearchStrategy :: SearchStrategy
locationSearchStrategy targetNounKey = do
  playerLocation <- getPlayerLocationM
  let objectSemanticMap = _objectSemanticMap playerLocation
  case Data.Map.Strict.lookup targetNounKey objectSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      let targetGID = Data.Set.elemAt 0 objSet
      -- Find what contains/supports this object
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      case Data.Map.Strict.lookup targetGID spatialMap of
        Just relationships -> do
          let sources = getContainerSources relationships
          case sources of
            (sourceGID:_) -> pure $ Just (targetGID, sourceGID)
            []            -> pure Nothing  -- Object exists but has no container
        Nothing -> pure Nothing
    _ -> pure Nothing
  where
    getContainerSources :: Set SpatialRelationship -> [GID Object]
    getContainerSources relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]
        {-
-- | Edge case: Search global perception map
perceptionSearchStrategy :: SearchStrategy
perceptionSearchStrategy targetNounKey = do
  world <- gets _world
  let perceptionMap = _perceptionMap world

  -- Convert NounKey to DirectionalStimulusNounPhrase for perception map lookup
  case nounKeyToDirectionalStimulus targetNounKey of
    Just dsnp -> case Data.Map.Strict.lookup dsnp perceptionMap of
      Just objSet | not (Data.Set.null objSet) -> do
        let targetGID = Data.Set.elemAt 0 objSet
        -- Find what contains/supports this object
        let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
        case Data.Map.Strict.lookup targetGID spatialMap of
          Just relationships -> do
            let sources = getContainerSources relationships
            case sources of
              (sourceGID:_) -> pure $ Just (targetGID, sourceGID)
              []            -> pure Nothing
          Nothing -> pure Nothing
      _ -> pure Nothing
    Nothing -> pure Nothing
  where
    getContainerSources :: Set SpatialRelationship -> [GID Object]
    getContainerSources relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]

    -- Helper to convert NounKey to DirectionalStimulusNounPhrase
    nounKeyToDirectionalStimulus :: NounKey -> DirectionalStimulusNounPhrase
    nounKeyToDirectionalStimulus (DirectionalStimulusKey dsn) =
      DirectionalStimulusNounPhrase (SimpleNounPhrase dsn)
--    nounKeyToDirectionalStimulus (ObjectiveKey obj) =
-}
