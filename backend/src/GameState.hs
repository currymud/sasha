module GameState ( addToInventoryM
                 , changeImplicit
                 , clearNarration
                 , getObjectM
                 , getActionManagementM
                 , getDescriptionM
                 , getInventoryObjectsM
                 , getLocationActionMapsM
                 , getLocationM
                 , getLocationObjectIDsM
                 , getPlayerM
                 , getPlayerLocationM
                 , getPlayerLocationGID
                 , modifyLocationMapM
                 , modifyLocationActionMapsM
                 , modifyLocationM
                 , modifyNarration
                 , modifyObjectM
                 , modifyObjectMapM
                 , modifySpatialRelationshipMapM
                 , modifySpatialRelationshipsForObjectM
                 , modifyObjectActionManagementM
                 , modifyPerceptionForStimulusM
                 , modifyWorldM
                 , addObjectToLocationSemanticMapM
                 , removeObjectFromLocationSemanticMapM
                 , modifyPerceptionMapM
                 , parseAcquisitionPhrase
                 , parseConsumptionPhrase
                 , processAcquisitionEffect
                 , processConsumptionEffect
                 , removeFromInventoryM
                 , updatePerceptionMapM,youSeeM) where
import           Control.Monad                 (filterM)
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (gets, modify')
import qualified Data.Bifunctor
import           Data.Map.Strict               (Map, elems)
import qualified Data.Map.Strict
import           Data.Set                      (Set, delete, empty, fromList,
                                                insert, member, null, toList)
import qualified Data.Set                      (filter, foldl', insert,
                                                singleton)
import           Data.Text                     (Text, intercalate, null, pack)
import           Error                         (throwMaybeM)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectKey (ObjectKey),
                                                ActionManagement (AAManagementKey, CAManagementKey, ISAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ConsumptionActionF (_consumptionAction),
                                                GameComputation,
                                                GameState (_narration, _player, _world),
                                                ImplicitStimulusActionF,
                                                Location (_locationActionManagement),
                                                Narration (Narration),
                                                Object (_description, _descriptives, _objectActionManagement),
                                                Player (_location, _playerActions),
                                                SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                                _objectSemanticMap,
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Mappings                (GIDToDataMap, _getGIDToDataMap)
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ConsumableNounPhrase (ConsumableNounPhrase),
                                                DirectionalStimulusNounPhrase,
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Model.Parser.GCase            (NounKey (ConsumableNounKey, ObjectiveKey))


getDescriptionM :: GID Object -> GameComputation Identity Text
getDescriptionM oid = do
  _description <$> getObjectM oid

parseAcquisitionPhrase :: AcquisitionVerbPhrase -> (ObjectPhrase,NounKey)
parseAcquisitionPhrase avp = Data.Bifunctor.second ObjectiveKey $ case avp of
  SimpleAcquisitionVerbPhrase _ ophrase ->
    let obj = case ophrase of
               (ObjectPhrase (SimpleNounPhrase obj'))             -> obj'
               (ObjectPhrase (NounPhrase _ obj'))                 -> obj'
               (ObjectPhrase (DescriptiveNounPhrase _ obj'))      -> obj'
               (ObjectPhrase (DescriptiveNounPhraseDet _ _ obj')) -> obj'
    in (ophrase, obj)
  _ -> error "get: unsupported AcquisitionVerbPhrase"

parseConsumptionPhrase :: ConsumptionVerbPhrase -> (ConsumableNounPhrase,NounKey)
parseConsumptionPhrase avp = Data.Bifunctor.second ConsumableNounKey $ case avp of
  ConsumptionVerbPhrase _ ophrase ->
    let obj = case ophrase of
               (ConsumableNounPhrase (SimpleNounPhrase obj'))             -> obj'
               (ConsumableNounPhrase (NounPhrase _ obj'))                 -> obj'
               (ConsumableNounPhrase (DescriptiveNounPhrase _ obj'))      -> obj'
               (ConsumableNounPhrase (DescriptiveNounPhraseDet _ _ obj')) -> obj'
    in (ophrase, obj)
-- GameState.hs - Updated effect processing functions

processAcquisitionEffect :: AcquisitionVerbPhrase
                         -> GID AcquisitionActionF
                         -> GameComputation Identity ()
processAcquisitionEffect avp newActionGID = do
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing acquisition action for this phrase
        filteredActions = Data.Set.filter (\case AAManagementKey p _ -> p /= avp; _ -> True) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (AAManagementKey avp newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }

processConsumptionEffect :: ConsumptionVerbPhrase
                         -> GID ConsumptionActionF
                         -> GameComputation Identity ()
processConsumptionEffect cvp newActionGID = do
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing consumption action for this phrase
        filteredActions = Data.Set.filter (\case CAManagementKey p _ -> p /= cvp; _ -> True) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (CAManagementKey cvp newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }
youSeeM :: GameComputation Identity ()
youSeeM = do
  playerLocation <- getPlayerLocationM
  let objectSemanticMap = _objectSemanticMap playerLocation

  -- Filter for ObjectiveKey entries and get their object ID sets
  let objectiveEntries = Data.Map.Strict.toList $ Data.Map.Strict.filterWithKey (\k _ -> isObjectiveKey k) objectSemanticMap
      objectiveOidSets = map snd objectiveEntries
      objectiveOids = concatMap Data.Set.toList objectiveOidSets

  -- Filter to only show surface-level objects (not supported by other objects in this location)
  surfaceLevelOids <- filterM isSurfaceLevel objectiveOids

  -- Get descriptions for surface-level objects and what they support
  surfaceDescriptions <- mapM getSurfaceObjectDescription surfaceLevelOids

  -- Add descriptions to narration if any objects found
  case filter (not . Data.Text.null) surfaceDescriptions of
    [] -> pure () -- No objectives to show
    descriptions -> do
      let seeText = "You see: " <> Data.Text.intercalate ", " descriptions
      modifyNarration $ updateActionConsequence seeText
  where
    isObjectiveKey :: NounKey -> Bool
    isObjectiveKey (ObjectiveKey _) = True
    isObjectiveKey _                = False

    -- Check if an object is surface-level (not nested more than 1 level deep)
    isSurfaceLevel :: GID Object -> GameComputation Identity Bool
    isSurfaceLevel oid = do
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      case Data.Map.Strict.lookup oid spatialMap of
        Nothing -> pure True -- No spatial relationships = surface level
        Just relationships -> do
          -- Check containment depth
          depth <- getContainmentDepth oid spatialMap 0
          pure (depth <= 1)  -- Only show objects at depth 0 (surface) or 1 (on surface objects)

    -- Calculate containment depth (how many levels deep an object is nested)
    getContainmentDepth :: GID Object -> Map (GID Object) (Set SpatialRelationship) -> Int -> GameComputation Identity Int
    getContainmentDepth oid spatialMap currentDepth = do
      case Data.Map.Strict.lookup oid spatialMap of
        Nothing -> pure currentDepth
        Just relationships -> do
          let containedInRelationships = filter isContainedIn (Data.Set.toList relationships)
              supportedByRelationships = filter isSupportedBy (Data.Set.toList relationships)
          case (containedInRelationships, supportedByRelationships) of
            (ContainedIn parentOid : _, _) ->
              -- If contained, recurse with increased depth
              getContainmentDepth parentOid spatialMap (currentDepth + 1)
            ([], SupportedBy parentOid : _) ->
              -- If only supported, recurse with same depth (supporting doesn't increase nesting)
              getContainmentDepth parentOid spatialMap currentDepth
            _ -> pure currentDepth

    isContainedIn :: SpatialRelationship -> Bool
    isContainedIn (ContainedIn _) = True
    isContainedIn _               = False

    isSupportedBy :: SpatialRelationship -> Bool
    isSupportedBy (SupportedBy _) = True
    isSupportedBy _               = False

    getSurfaceObjectDescription :: GID Object -> GameComputation Identity Text
    getSurfaceObjectDescription oid = do
      obj <- getObjectM oid
      supportedDesc <- getSupportedObjectsDescription oid
      let baseDesc = _description obj
      pure $ if Data.Text.null supportedDesc
             then baseDesc
             else baseDesc <> supportedDesc

    getSupportedObjectsDescription :: GID Object -> GameComputation Identity Text
    getSupportedObjectsDescription supporterOid = do
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      case Data.Map.Strict.lookup supporterOid spatialMap of
        Nothing -> pure ""
        Just relationships -> do
          -- Only show supported objects (things "on" the surface)
          -- Don't show contained objects (things "in" containers) at this level
          supportedObjects <- mapM processSupportRelationship (Data.Set.toList relationships)
          let validSupported = filter (not . Data.Text.null) supportedObjects
              supportedText = case validSupported of
                []        -> ""
                supported -> ". On it: " <> Data.Text.intercalate ", " supported
          pure supportedText

    processSupportRelationship :: SpatialRelationship -> GameComputation Identity Text
    processSupportRelationship (Supports oidSet) = do
      descriptions <- mapM getObjectDescription (Data.Set.toList oidSet)
      pure $ Data.Text.intercalate ", " descriptions
    processSupportRelationship _ = pure ""

    getObjectDescription :: GID Object -> GameComputation Identity Text
    getObjectDescription oid = do
      obj <- getObjectM oid
      pure $ _description obj

-- General function to get objects by spatial relationship
getObjectsBySpatialRelationship :: SpatialRelationship -> GameComputation Identity [GID Object]
getObjectsBySpatialRelationship targetRelationship = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      matchingOids = [oid | (oid, relationships) <- Data.Map.Strict.toList spatialMap,
                           targetRelationship `Data.Set.member` relationships]
  pure matchingOids

-- Inventory-specific convenience functions
getInventoryObjectsM :: GameComputation Identity [GID Object]
getInventoryObjectsM = getObjectsBySpatialRelationship Inventory

updatePerceptionMapM :: GID Object
                       -> GameComputation Identity ()
updatePerceptionMapM oid = do
  obj <- getObjectM oid
  let descriptivesList = Data.Set.toList $ _descriptives obj
  mapM_ addDescriptiveToPerceptionMap descriptivesList
  where
    addDescriptiveToPerceptionMap :: DirectionalStimulusNounPhrase -> GameComputation Identity ()
    addDescriptiveToPerceptionMap dsnp = do
      modifyPerceptionMapM $ \perceptionMap ->
        let currentSet = Data.Map.Strict.findWithDefault Data.Set.empty dsnp perceptionMap
            updatedSet = Data.Set.insert oid currentSet
        in Data.Map.Strict.insert dsnp updatedSet perceptionMap

getLocationObjectIDsM :: GID Location -> GameComputation Identity (Set ActionEffectKey)
getLocationObjectIDsM lid = do
  location <- getLocationM lid
  let objectSemanticMap = _objectSemanticMap location
      objectGIDSets = Data.Map.Strict.elems objectSemanticMap
      allObjectGIDs = concatMap Data.Set.toList objectGIDSets
  pure $ Data.Set.fromList (map ObjectKey allObjectGIDs)

changeImplicit :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> GameComputation Identity ()
changeImplicit verb newActionGID = do
  playerLocationGID <- getPlayerLocationGID
  modifyLocationM playerLocationGID $ \loc ->
    let currentActions = _locationActionManagement loc
        -- Remove old action for this verb and add new one
        ActionManagementFunctions actionSet = currentActions
        filteredActions = Data.Set.filter (\case ISAManagementKey v _ -> v /= verb; _ -> True) actionSet
        updatedActions = Data.Set.insert (ISAManagementKey verb newActionGID) filteredActions
    in loc { _locationActionManagement = ActionManagementFunctions updatedActions }

-- There is some cruft forming
getPlayerLocationGID :: GameComputation Identity (GID Location)
getPlayerLocationGID =
  _location <$> getPlayerM

getLocationM :: GID Location -> GameComputation Identity Location
getLocationM lid = do
  locationMap <- gets (_getGIDToDataMap . _locationMap . _world)
  throwMaybeM ("Location not found in location map" <> pack (show lid))
    $ Data.Map.Strict.lookup lid locationMap

getPlayerM :: GameComputation Identity Player
getPlayerM = gets _player

getObjectM :: GID Object -> GameComputation Identity Object
getObjectM oid = do
  objMap <- gets (_getGIDToDataMap . _objectMap . _world)
  throwMaybeM ("Object not found in object map" <> pack (show oid)) $ Data.Map.Strict.lookup oid objMap

getActionManagementM :: GID Object -> GameComputation Identity ActionManagementFunctions
getActionManagementM oid = _objectActionManagement <$> getObjectM oid

getPlayerLocationM :: GameComputation Identity Location
getPlayerLocationM = do
  location <- _location <$> getPlayerM
  locationMap <- gets (_getGIDToDataMap . _locationMap . _world)
  throwMaybeM "Player location not found" $ Data.Map.Strict.lookup location locationMap

getLocationActionMapsM :: GID Location -> GameComputation Identity ActionManagementFunctions
getLocationActionMapsM lid = do
  world <- gets _world
  let locationMap = _getGIDToDataMap $ _locationMap world
  location <- throwMaybeM "location not found" $ Data.Map.Strict.lookup lid locationMap
  return $ _locationActionManagement location

modifyLocationActionMapsM :: (ActionManagementFunctions -> ActionManagementFunctions)
                        -> GID Location
                        -> GameComputation Identity ()
modifyLocationActionMapsM actionF lid = do
  world <- gets _world
  let locationMap = _getGIDToDataMap $ _locationMap world
  location <- throwMaybeM "location not found" $ Data.Map.Strict.lookup lid locationMap
  let currentActionManagement = _locationActionManagement location
      updatedActionManagement = actionF currentActionManagement
      updatedLocation = location { _locationActionManagement = updatedActionManagement }
      updatedLocationMap = Data.Map.Strict.insert lid updatedLocation locationMap
      updatedWorld = world { _locationMap = (_locationMap world) { _getGIDToDataMap = updatedLocationMap } }
  modify' (\gs -> gs { _world = updatedWorld })

modifyLocationM :: GID Location
                -> (Location -> Location)
                -> GameComputation Identity ()
modifyLocationM lid locationF = do
  world <- gets _world
  let locationMap = _getGIDToDataMap $ _locationMap world
  location <- throwMaybeM ("Location not found: " <> pack (show lid)) $
              Data.Map.Strict.lookup lid locationMap
  let updatedLocation = locationF location
      updatedLocationMap = Data.Map.Strict.insert lid updatedLocation locationMap
      updatedWorld = world { _locationMap = (_locationMap world) { _getGIDToDataMap = updatedLocationMap } }
  modify' (\gs -> gs { _world = updatedWorld })

modifyLocationMapM :: (GIDToDataMap Location Location -> GIDToDataMap Location Location)
                   -> GameComputation Identity ()
modifyLocationMapM locationMapF = do
  world <- gets _world
  let currentLocationMap = _locationMap world
      updatedLocationMap = locationMapF currentLocationMap
      updatedWorld = world { _locationMap = updatedLocationMap }
  modify' (\gs -> gs { _world = updatedWorld })

modifyObjectMapM :: (GIDToDataMap Object Object -> GIDToDataMap Object Object)
                 -> GameComputation Identity ()
modifyObjectMapM objectMapF = do
  world <- gets _world
  let currentObjectMap = _objectMap world
      updatedObjectMap = objectMapF currentObjectMap
      updatedWorld = world { _objectMap = updatedObjectMap }
  modify' (\gs -> gs { _world = updatedWorld })

modifyPerceptionForStimulusM :: DirectionalStimulusNounPhrase
                             -> (Set (GID Object) -> Set (GID Object))
                             -> GameComputation Identity ()
modifyPerceptionForStimulusM stimulus setF = do
  world <- gets _world
  let perceptionMap = _perceptionMap world
      currentSet = Data.Map.Strict.findWithDefault mempty stimulus perceptionMap
      updatedSet = setF currentSet
      updatedPerceptionMap = Data.Map.Strict.insert stimulus updatedSet perceptionMap
      updatedWorld = world { _perceptionMap = updatedPerceptionMap }
  modify' (\gs -> gs { _world = updatedWorld })

modifyNarration :: (Narration -> Narration)
                     -> GameComputation Identity ()
modifyNarration narrationF = do
  current_narration <- gets _narration
  let updatedNarrative = narrationF current_narration
  modify' (\gs -> gs{ _narration = updatedNarrative })

modifyObjectM :: GID Object
              -> (Object -> Object)
              -> GameComputation Identity ()
modifyObjectM oid objectF = do
  world <- gets _world
  let objectMap = _getGIDToDataMap $ _objectMap world
  object <- throwMaybeM ("Object not found: " <> pack (show oid)) $
            Data.Map.Strict.lookup oid objectMap
  let updatedObject = objectF object
      updatedObjectMap = Data.Map.Strict.insert oid updatedObject objectMap
      updatedWorld = world { _objectMap = (_objectMap world) { _getGIDToDataMap = updatedObjectMap } }
  modify' (\gs -> gs { _world = updatedWorld })

modifyPerceptionMapM :: (Map DirectionalStimulusNounPhrase (Set (GID Object)) -> Map DirectionalStimulusNounPhrase (Set (GID Object)))
                     -> GameComputation Identity ()
modifyPerceptionMapM perceptionMapF = do
  world <- gets _world
  let currentPerceptionMap = _perceptionMap world
      updatedPerceptionMap = perceptionMapF currentPerceptionMap
      updatedWorld = world { _perceptionMap = updatedPerceptionMap }
  modify' (\gs -> gs { _world = updatedWorld })

modifySpatialRelationshipMapM :: (SpatialRelationshipMap -> SpatialRelationshipMap)
                              -> GameComputation Identity ()
modifySpatialRelationshipMapM spatialMapF = do
  world <- gets _world
  let currentSpatialMap = _spatialRelationshipMap world
      updatedSpatialMap = spatialMapF currentSpatialMap
      updatedWorld = world { _spatialRelationshipMap = updatedSpatialMap }
  modify' (\gs -> gs { _world = updatedWorld })

-- Modify spatial relationships for a specific object
modifySpatialRelationshipsForObjectM :: GID Object
                                     -> (Set SpatialRelationship -> Set SpatialRelationship)
                                     -> GameComputation Identity ()
modifySpatialRelationshipsForObjectM oid relationshipSetF = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      currentRelationships = Data.Map.Strict.findWithDefault mempty oid spatialMap
      updatedRelationships = relationshipSetF currentRelationships
      updatedSpatialMap = Data.Map.Strict.insert oid updatedRelationships spatialMap
      updatedWorld = world { _spatialRelationshipMap = SpatialRelationshipMap updatedSpatialMap }
  modify' (\gs -> gs { _world = updatedWorld })

-- Modify the entire world
modifyWorldM :: (World -> World) -> GameComputation Identity ()
modifyWorldM worldF = do
  currentWorld <- gets _world
  let updatedWorld = worldF currentWorld
  modify' (\gs -> gs { _world = updatedWorld })

-- Additional utility functions for common operations

-- Add an object to a location's semantic map
addObjectToLocationSemanticMapM :: GID Location
                                -> NounKey
                                -> GID Object
                                -> GameComputation Identity ()
addObjectToLocationSemanticMapM lid nounKey oid =
  modifyLocationM lid $ \loc ->
    let currentMap = _objectSemanticMap loc
        currentSet = Data.Map.Strict.findWithDefault Data.Set.empty nounKey currentMap
        updatedSet = Data.Set.insert oid currentSet
        updatedMap = Data.Map.Strict.insert nounKey updatedSet currentMap
    in loc { _objectSemanticMap = updatedMap }

-- Remove an object from a location's semantic map
removeObjectFromLocationSemanticMapM :: GID Location
                                     -> NounKey
                                     -> GID Object
                                     -> GameComputation Identity ()
removeObjectFromLocationSemanticMapM lid nounKey oid =
  modifyLocationM lid $ \loc ->
    let currentMap = _objectSemanticMap loc
        currentSet = Data.Map.Strict.findWithDefault Data.Set.empty nounKey currentMap
        updatedSet = Data.Set.delete oid currentSet
        updatedMap = if Data.Set.null updatedSet
                    then Data.Map.Strict.delete nounKey currentMap
                    else Data.Map.Strict.insert nounKey updatedSet currentMap
    in loc { _objectSemanticMap = updatedMap }

modifyObjectActionManagementM :: GID Object
                              -> (ActionManagementFunctions -> ActionManagementFunctions)
                              -> GameComputation Identity ()
modifyObjectActionManagementM oid actionF =
  modifyObjectM oid $ \obj ->
    obj { _objectActionManagement = actionF (_objectActionManagement obj) }

clearNarration :: GameComputation Identity ()
clearNarration = modifyNarration (const emptyNarration)
  where
    emptyNarration :: Narration
    emptyNarration = Narration mempty mempty

-- Add object to inventory (replaces current inventory insertion)
addToInventoryM :: GID Object -> GameComputation Identity ()
addToInventoryM oid = do
  modifySpatialRelationshipsForObjectM oid (Data.Set.insert Inventory)

-- Remove object from inventory
removeFromInventoryM :: GID Object -> GameComputation Identity ()
removeFromInventoryM oid = do
  modifySpatialRelationshipsForObjectM oid (Data.Set.delete Inventory)
