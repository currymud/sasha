module GameState ( changeImplicit, clearNarration
                 , getObjectM
                 , getActionManagementM
                 , getLocationActionMapsM
                 , getLocationM
                 , getLocationObjectIDsM
                 , getPlayerM
                 , getPlayerActionsM
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
                 , updatePerceptionMapM,youSeeM) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (gets, modify')
import           Data.Map.Strict               (Map, elems)
import qualified Data.Map.Strict
import           Data.Set                      (Set, empty, fromList, insert,
                                                member, null, toList)
import           Data.Text                     (Text, intercalate, pack)
import           Error                         (throwMaybeM)
import           Model.GameState               (ActionEffectKey (ObjectKey),
                                                ActionManagement (_implicitStimulusActionManagement),
                                                GameComputation,
                                                GameState (_narration, _player, _world),
                                                ImplicitStimulusActionF,
                                                Location (_locationActionManagement),
                                                Narration (Narration),
                                                Object (_description, _descriptives, _objectActionManagement),
                                                Player (_location, _playerActions),
                                                PlayerActions,
                                                SpatialRelationship,
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                                _objectSemanticMap,
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Mappings                (GIDToDataMap, _getGIDToDataMap)
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase)
import           Model.Parser.GCase            (NounKey (ObjectiveKey))
  {-
youSeeM :: GameComputation Identity ()
youSeeM = do
  objectSemanticMap <- _objectSemanticMap <$> getPlayerLocationM
-}
youSeeM :: Set (GID Object) -> GameComputation Identity ()
youSeeM objectIds = do
  location <- getPlayerLocationM
  let objectSemanticMap = _objectSemanticMap location

  -- Filter objectIds to only include those that are ObjectiveKey entries
  -- We need to check which objects in the location are objectives
  let objectiveObjects = Data.Map.Strict.foldlWithKey'
        (\acc nounKey objectId ->
          case nounKey of
            ObjectiveKey _ -> if Data.Set.member objectId objectIds
                            then Data.Set.insert objectId acc
                            else acc
            _ -> acc
        ) Data.Set.empty objectSemanticMap

  if Data.Set.null objectiveObjects
    then pure () -- Don't show anything if no objectives are visible
    else do
      -- Get object descriptions for visible objectives
      objects <- mapM getObjectM (Data.Set.toList objectiveObjects)
      let descriptions = map _description objects

      -- Create a combined message showing what the player sees
      let seeMessage = case descriptions of
            []       -> "You don't see anything of interest here."
            [single] -> "You see: " <> single
            multiple -> "You see: " <> Data.Text.intercalate ", " multiple
      modifyNarration $ updateActionConsequence seeMessage

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
getLocationObjectIDsM lid =
  Data.Set.fromList . fmap ObjectKey . elems . _objectSemanticMap <$> getLocationM lid

changeImplicit :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> GameComputation Identity ()
changeImplicit verb newActionGID = do
  playerLocationGID <- getPlayerLocationGID
  modifyLocationM playerLocationGID $ \loc ->
    let actionMgmt = _locationActionManagement loc
        implicitMap = _implicitStimulusActionManagement actionMgmt
        updatedImplicitMap = Data.Map.Strict.insert verb newActionGID implicitMap
        updatedActionMgmt = actionMgmt { _implicitStimulusActionManagement = updatedImplicitMap }
    in loc { _locationActionManagement = updatedActionMgmt }
-- There is some cruft forming
getPlayerLocationGID :: GameComputation Identity (GID Location)
getPlayerLocationGID =
  _location <$> getPlayerM

getLocationM :: GID Location -> GameComputation Identity Location
getLocationM lid = do
  locationMap <- gets (_getGIDToDataMap . _locationMap . _world)
  throwMaybeM ("Location not found in location map" <> pack (show lid))
    $ Data.Map.Strict.lookup lid locationMap

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
