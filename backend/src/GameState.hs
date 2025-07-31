module GameState ( clearNarration
                 , getObjectM
                 , getActionManagementM
                 , getLocationActionMapsM
                 , getPlayerM
                 , getPlayerActionsM
                 , getPlayerLocationM
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
                 , modifyPerceptionMapM) where
import           Control.Monad.Identity     (Identity)
import           Control.Monad.State        (gets, modify')
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict
import           Data.Set                   (Set)
import           Data.Text                  (pack)
import           Error                      (throwMaybeM)
import           Model.GameState            (ActionManagement, GameComputation,
                                             GameState (_narration, _player, _world),
                                             Location (_locationActionManagement),
                                             Narration (Narration),
                                             Object (_objectActionManagement),
                                             Player (_location, _playerActions),
                                             PlayerActions, SpatialRelationship,
                                             SpatialRelationshipMap (SpatialRelationshipMap),
                                             World (_locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                             _objectSemanticMap)
import           Model.GID                  (GID)
import           Model.Mappings             (GIDToDataMap, _getGIDToDataMap)
import           Model.Parser.Atomics.Nouns (DirectionalStimulus)
import           Model.Parser.GCase         (NounKey)

getPlayerActionsM :: GameComputation Identity  PlayerActions
getPlayerActionsM =  _playerActions <$> getPlayerM

getPlayerM :: GameComputation Identity Player
getPlayerM = gets _player

getObjectM :: GID Object -> GameComputation Identity Object
getObjectM oid = do
  objMap <- gets (_getGIDToDataMap . _objectMap . _world)
  throwMaybeM ("Object not found in object map" <> pack (show oid)) $ Data.Map.Strict.lookup oid objMap

getActionManagementM :: GID Object -> GameComputation Identity ActionManagement
getActionManagementM oid = _objectActionManagement <$> getObjectM oid

getPlayerLocationM :: GameComputation Identity Location
getPlayerLocationM = do
  location <- _location <$> getPlayerM
  locationMap <- gets (_getGIDToDataMap . _locationMap . _world)
  throwMaybeM "Player location not found" $ Data.Map.Strict.lookup location locationMap

getLocationActionMapsM :: GID Location
                        -> GameComputation Identity ActionManagement
getLocationActionMapsM lid = do
  world <- gets _world
  let locationMap = _getGIDToDataMap $ _locationMap world
  location <- throwMaybeM "location not found" $ Data.Map.Strict.lookup lid locationMap
  return $ _locationActionManagement location

modifyLocationActionMapsM :: (ActionManagement -> ActionManagement)
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

modifyPerceptionForStimulusM :: DirectionalStimulus
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

modifyPerceptionMapM :: (Map DirectionalStimulus (Set (GID Object)) -> Map DirectionalStimulus (Set (GID Object)))
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
    loc { _objectSemanticMap = Data.Map.Strict.insert nounKey oid (_objectSemanticMap loc) }

-- Remove an object from a location's semantic map
removeObjectFromLocationSemanticMapM :: GID Location
                                     -> NounKey
                                     -> GameComputation Identity ()
removeObjectFromLocationSemanticMapM lid nounKey =
  modifyLocationM lid $ \loc ->
    loc { _objectSemanticMap = Data.Map.Strict.delete nounKey (_objectSemanticMap loc) }

-- Update object action management
modifyObjectActionManagementM :: GID Object
                              -> (ActionManagement -> ActionManagement)
                              -> GameComputation Identity ()
modifyObjectActionManagementM oid actionF =
  modifyObjectM oid $ \obj ->
    obj { _objectActionManagement = actionF (_objectActionManagement obj) }
clearNarration :: GameComputation Identity ()
clearNarration = modifyNarration (const emptyNarration)
  where
    emptyNarration :: Narration
    emptyNarration = Narration mempty mempty
