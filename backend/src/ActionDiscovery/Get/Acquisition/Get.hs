module ActionDiscovery.Get.Acquisition.Get (manageAcquisitionProcess, manageAcquisitionProcessRoleBased) where

import           Control.Monad.Error.Class                        (throwError)
import           Control.Monad.Identity                           (Identity)
import           Control.Monad.Reader.Class                       (asks)
import           Control.Monad.State                              (gets)
import qualified Data.Map.Strict
import           Data.Set                                         (Set, elemAt,
                                                                   filter,
                                                                   member, null,
                                                                   toList)
import qualified Data.Text
import           GameState                                        (getObjectM,
                                                                   getPlayerLocationM,
                                                                   getPlayerM,
                                                                   parseAcquisitionPhrase)
import           GameState.ActionManagement                       (findAVKey,
                                                                   lookupAcquisitionPhrase,
                                                                   processEffectsFromRegistry)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Core                                       (AcquisitionActionF (AcquisitionActionF, CannotAcquireF, CollectedF, LosesObjectF, ObjectNotGettableF),
                                                                   AgentAcquisitionActionF,
                                                                   AcquisitionVerbActionMap,
                                                                   AgentAcquisitionActionMap,
                                                                   ActionEffectKey (AcquisitionalActionKey),
                                                                   ActionMaps (_acquisitionActionMap, _agentAcquisitionActionMap),
                                                                   Config (_actionMaps),
                                                                   CoordinationResult (CoordinationResult),
                                                                   GameComputation,
                                                                   GameState (_world),
                                                                   Location (_locationInventory, _objectSemanticMap),
                                                                   Object (_objectActionManagement),
                                                                   Player (_playerActions),
                                                                   SearchStrategy,
                                                                   SpatialRelationship (ContainedIn, SupportedBy),
                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                   World (_globalSemanticMap, _spatialRelationshipMap))
import           Model.GID                                        (GID)
import           Model.Parser.Composites.Verbs                    (AcquisitionVerbPhrase)


-- ToDo: Add Location related values,
-- Location effects need to be included in the process
-- Role-based acquisition process (preferred - no error-prone pattern matching)
-- Currently just redirects to the old system since we're using conversion functions
-- This will be fully implemented when we remove the conversion layer
manageAcquisitionProcessRoleBased :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcessRoleBased = manageAcquisitionProcess
  -- Note: Full role-based implementation will be added when we remove conversion functions
  -- For now, the role-based types are used at the declaration level (in SashaDemo.hs)
  -- and converted back to AcquisitionActionF for compatibility

-- Original acquisition process (kept for backwards compatibility)
manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupActionF availableActions of
    Nothing -> error "Programmer Error: No acquisition action found for phrase: "
    Just actionGID -> do
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No acquisition action found for GID: "
        Just foundAction -> do
          let actionEffectKey = AcquisitionalActionKey actionGID
          case foundAction of
            (AcquisitionActionF actionFunc) -> do
               actionFunc
                 actionEffectKey
                 lookupActionF
                 (lookupAcquisitionAction actionMap)
                 arRes
            (CannotAcquireF actionF) -> actionF actionEffectKey
            (LosesObjectF _) ->
              error "LosesObjectF should not be in player action map"
            (ObjectNotGettableF _) ->
              error "ObjectNotGettableF should not be in player action map"
            (CollectedF _) ->
              error "CollectedF should not be in player action map"
  where
    arRes = parseAcquisitionPhrase avp
    lookupActionF = lookupAcquisitionPhrase avp
-- | General case: Search global semantic map, verify in location inventory
lookupAcquisitionAction :: AcquisitionVerbActionMap
                             -> GID Object
                             -> GameComputation Identity AcquisitionActionF
lookupAcquisitionAction actionMap oid = do
  actionMgmt <- _objectActionManagement <$> getObjectM oid
  case findAVKey get actionMgmt of
    Nothing -> throwError $ (Data.Text.pack . show) oid <> " does not have a 'get' action."
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> throwError $ "No acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> pure action

locationSearchStrategy :: SearchStrategy
locationSearchStrategy targetNounKey = do
  world <- gets _world
  playerLocation <- getPlayerLocationM
  let globalSemanticMap = _globalSemanticMap world
      locationInventory = _locationInventory playerLocation
  case Data.Map.Strict.lookup targetNounKey globalSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      -- Find first object that's in the current location's inventory
      let availableObjects = Data.Set.filter (`Data.Set.member` locationInventory) objSet
      if not (Data.Set.null availableObjects)
        then do
          let targetGID = Data.Set.elemAt 0 availableObjects
          -- Find what contains/supports this object
          let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
          case Data.Map.Strict.lookup targetGID spatialMap of
            Just relationships -> do
              let sources = getContainerSources relationships
              case sources of
                (sourceGID:_) -> pure $ Just (targetGID, sourceGID)
                []            -> pure Nothing  -- Object exists but has no container
            Nothing -> pure Nothing
        else pure Nothing  -- Object exists but not in this location
    _ -> pure Nothing
  where
    getContainerSources :: Set SpatialRelationship -> [GID Object]
    getContainerSources relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]
finalizeAcquisition :: ActionEffectKey
                        -> GID Object
                        -> GID Object
                        -> GameComputation Identity CoordinationResult
                        -> (GID Object -> GameComputation Identity CoordinationResult)
                        -> GameComputation Identity ()
finalizeAcquisition actionEffectKey containerGID objectGID objectActionF containerActionF = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
  case Data.Map.Strict.lookup objectGID spatialMap of
   Nothing -> throwError $ "No spatial relationships found for object " <> (Data.Text.pack . show) objectGID
   -- ToDo move relationships higher up, we can find out sooner.
   Just relationships -> do
     let isContainedInSource = any (\case
           ContainedIn oid -> oid == containerGID
           SupportedBy oid -> oid == containerGID
           _ -> False) (Data.Set.toList relationships)
     if not isContainedInSource
     then throwError $ "Object " <> (Data.Text.pack . show) objectGID <> " is not in or on container " <> (Data.Text.pack . show) containerGID
     else  do
       (CoordinationResult playerGetObjectF objectEffects) <- objectActionF
       (CoordinationResult containerRemoveObjectF containerEffects) <- containerActionF objectGID
       let allEffects = actionEffectKey:(objectEffects <> containerEffects)
       mapM_ processEffectsFromRegistry allEffects >> containerRemoveObjectF >> playerGetObjectF
