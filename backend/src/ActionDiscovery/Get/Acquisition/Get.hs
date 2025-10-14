module ActionDiscovery.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Control.Applicative                              ((<|>))
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
import           GameState.ActionManagement                       (findAgentAVKey,
                                                                   findAgentAAKey,
                                                                   findObjectAVKey,
                                                                   findContainerAVKey,
                                                                   processEffectsFromRegistry)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Core                                       (AgentAcquisitionActionF (AgentAcquiresF, AgentCannotAcquireF),
                                                                   ObjectAcquisitionActionF (ObjectCollectedF, ObjectNotCollectableF),
                                                                   ContainerAcquisitionActionF (ContainerLosesObjectF, ContainerCannotReleaseF),
                                                                   AgentAcquisitionActionMap,
                                                                   ObjectAcquisitionActionMap,
                                                                   ContainerAcquisitionActionMap,
                                                                   ActionEffectKey (AgentAcquisitionalActionKey),
                                                                   ActionMaps (_agentAcquisitionActionMap, _objectAcquisitionActionMap, _containerAcquisitionActionMap),
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
-- Type-safe role-based acquisition process - eliminates DSL programmer errors
manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  
  -- Try to find a role-based agent action - first by verb phrase, then by verb
  let agentGIDMaybe = findAgentAAKey avp availableActions <|> findAgentAVKey get availableActions
  case agentGIDMaybe of
    Just agentGID -> do
      -- Use the role-based agent action map
      agentActionMap <- asks (_agentAcquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup agentGID agentActionMap of
        Just agentAction -> do
          let actionEffectKey = AgentAcquisitionalActionKey agentGID
          -- Execute the role-based agent action directly - no error-prone pattern matching!
          case agentAction of
            AgentAcquiresF acquisitionF -> do
              -- Type-safe agent acquisition - no programmer errors possible here!
              acquisitionF actionEffectKey arRes
            AgentCannotAcquireF actionF -> actionF actionEffectKey
        Nothing -> 
          -- Agent action not found in role-based map
          throwError "No agent acquisition action found for this action."
    Nothing -> 
      -- No role-based agent action found
      throwError "No agent acquisition action available for this action."
  where
    arRes = parseAcquisitionPhrase avp

-- | Role-based acquisition action lookup - coordinates object and container actions
-- This function is no longer needed since we removed AcquisitionActionF
-- lookupRoleBasedAcquisitionAction :: AcquisitionVerbPhrase 
--                                 -> GID Object 
--                                 -> GameComputation Identity AcquisitionActionF
-- This function is no longer needed since we removed AcquisitionActionF
{-
lookupRoleBasedAcquisitionAction avp oid = do
  -- This compatibility bridge is no longer needed
  throwError "AcquisitionActionF compatibility layer removed"
-}


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
