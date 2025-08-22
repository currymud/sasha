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
                 , modifyPerceptionForStimulusM
                 , modifyWorldM
                 , addObjectToLocationSemanticMapM
                 , removeObjectFromLocationSemanticMapM
                 , parseObjectPhrase
                 , parseConsumablePhrase
                 , parseDirectionalStimulusNounPhrase
                 , parseSupportPhrase
                 , parseAcquisitionPhrase
                 , parseConsumptionPhrase
                 , processAcquisitionEffect
                 , processConsumptionEffect
                 , processPosturalEffect
                 , removeFromInventoryM
                 ) where
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
import           Debug.Trace                   (trace)
import           Error                         (throwMaybeM)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectKey (ObjectKey),
                                                ActionManagement (AAManagementKey, CAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ConsumptionActionF (_consumptionAction),
                                                GameComputation,
                                                GameState (_narration, _player, _world),
                                                ImplicitStimulusActionF,
                                                Location (_locationActionManagement),
                                                Narration (Narration),
                                                Object (_description, _descriptives, _objectActionManagement),
                                                Player (_location, _playerActions),
                                                PosturalActionF,
                                                SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                                _objectSemanticMap,
                                                updateActionConsequence)
import           Model.GameState.Mappings      (GIDToDataMap, _getGIDToDataMap)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Consumable, Container, Surface)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ConsumableNounPhrase (ConsumableNounPhrase),
                                                ContainerPhrase (ContainerPhrase, SimpleContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                ObjectPhrase (ObjectPhrase),
                                                SupportPhrase (ContainerSupport, SurfaceSupport),
                                                SurfacePhrase (SimpleSurfacePhrase, SurfacePhrase))
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase),
                                                PosturalVerbPhrase (NegativePosturalVerbPhrase, PositivePosturalVerbPhrase))
import           Model.Parser.GCase            (NounKey (ConsumableNounKey, ContainerKey, DirectionalStimulusKey, ObjectiveKey, SurfaceKey))


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


parseDirectionalStimulusNounPhrase :: DirectionalStimulusNounPhrase -> NounKey
parseDirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase nounPhrase) =
  let noun = case nounPhrase of
               SimpleNounPhrase obj             -> obj
               NounPhrase _ obj                 -> obj
               DescriptiveNounPhrase _ obj      -> obj
               DescriptiveNounPhraseDet _ _ obj -> obj
  in DirectionalStimulusKey noun

parseObjectPhrase :: ObjectPhrase -> NounKey
parseObjectPhrase (ObjectPhrase nounPhrase) =
  let noun = case nounPhrase of
               SimpleNounPhrase obj             -> obj
               NounPhrase _ obj                 -> obj
               DescriptiveNounPhrase _ obj      -> obj
               DescriptiveNounPhraseDet _ _ obj -> obj
  in ObjectiveKey noun

parseConsumablePhrase :: ConsumableNounPhrase -> NounKey
parseConsumablePhrase (ConsumableNounPhrase phrase) =
  let noun = case phrase of
                     SimpleNounPhrase noun'             -> noun'
                     NounPhrase _ noun'                 -> noun'
                     DescriptiveNounPhrase _ noun'      -> noun'
                     DescriptiveNounPhraseDet _ _ noun' -> noun'
  in ConsumableNounKey noun

parseSupportPhrase :: SupportPhrase -> NounKey
parseSupportPhrase supportPhrase = case supportPhrase of
  SurfaceSupport surfacePhrase ->
    case surfacePhrase of
      SimpleSurfacePhrase nounPhrase -> extractSurfaceNoun nounPhrase
      SurfacePhrase _ nounPhrase     -> extractSurfaceNoun nounPhrase
  ContainerSupport containerPhrase ->
    case containerPhrase of
      SimpleContainerPhrase nounPhrase -> extractContainerNoun nounPhrase
      ContainerPhrase _ nounPhrase     -> extractContainerNoun nounPhrase
  where
    extractSurfaceNoun :: NounPhrase Surface -> NounKey
    extractSurfaceNoun nounPhrase = case nounPhrase of
      SimpleNounPhrase surface             -> SurfaceKey surface
      NounPhrase _ surface                 -> SurfaceKey surface
      DescriptiveNounPhrase _ surface      -> SurfaceKey surface
      DescriptiveNounPhraseDet _ _ surface -> SurfaceKey surface

    extractContainerNoun :: NounPhrase Container -> NounKey
    extractContainerNoun nounPhrase = case nounPhrase of
      SimpleNounPhrase container             -> ContainerKey container
      NounPhrase _ container                 -> ContainerKey container
      DescriptiveNounPhrase _ container      -> ContainerKey container
      DescriptiveNounPhraseDet _ _ container -> ContainerKey container
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

processPosturalEffect :: PosturalVerbPhrase
                      -> GID PosturalActionF
                      -> GameComputation Identity ()
processPosturalEffect posturalPhrase newActionGID = do
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing postural action for this phrase's verb
        filteredActions = Data.Set.filter (filterPosturalAction posturalPhrase) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (createPosturalManagementKey posturalPhrase newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }
  where
    filterPosturalAction phrase action = case (phrase, action) of
      (PositivePosturalVerbPhrase verb _, PPManagementKey v _) -> v /= verb
      (NegativePosturalVerbPhrase verb _, NPManagementKey v _) -> v /= verb
      _                                                        -> True

    createPosturalManagementKey phrase gid = case phrase of
      PositivePosturalVerbPhrase verb _ -> PPManagementKey verb gid
      NegativePosturalVerbPhrase verb _ -> NPManagementKey verb gid

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
    {-
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
-}

modifyObjectM :: GID Object
              -> (Object -> Object)
              -> GameComputation Identity ()
modifyObjectM oid objectF = do
  world <- gets _world
  let objectMap = _getGIDToDataMap $ _objectMap world
  trace ("modifyObjectM: Looking for object " ++ show oid ++ " in map with keys: " ++ show (Data.Map.Strict.keys objectMap)) $ pure ()
  object <- throwMaybeM ("Object not found: " <> pack (show oid)) $
            Data.Map.Strict.lookup oid objectMap
  trace ("modifyObjectM: Found object " ++ show oid ++ ", applying function") $ pure ()
  let updatedObject = objectF object
  trace ("modifyObjectM: Applied function, updating map") $ pure ()
  let updatedObjectMap = Data.Map.Strict.insert oid updatedObject objectMap
      updatedWorld = world { _objectMap = (_objectMap world) { _getGIDToDataMap = updatedObjectMap } }
  modify' (\gs -> gs { _world = updatedWorld })
  trace ("modifyObjectM: Successfully updated object " ++ show oid) $ pure ()

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
