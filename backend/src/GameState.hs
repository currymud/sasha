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
                 , modifyPlayerM
                 , modifySpatialRelationshipMapM
                 , modifySpatialRelationshipsForObjectM
                 , modifyPerceptionForStimulusM
                 , modifyWorldM
                 , addObjectToLocationSemanticMapM
                 , removeObjectFromLocationSemanticMapM
                 , parseAccessPhrase
                 , parseObjectPhrase
                 , parseConsumablePhrase
                 , parseContainerPhrase
                 , parseDirectionalStimulusNounPhrase
                 , parseSupportPhrase
                 , parseInstrumentalAccessNounPhrase
                 , parseAcquisitionPhrase
                 , parseConsumptionPhrase
                 , processSimpleContainerAccessEffect
                 , processAcquisitionEffect
                 , processPositivePosturalEffect
                 , processNegativePosturalEffect
                 , processConsumptionEffect
                 , removeFromInventoryM
                 , updateActionConsequence
                 ) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (gets, modify')
import qualified Data.Bifunctor
import           Data.Map.Strict               (elems)
import qualified Data.Map.Strict
import           Data.Set                      (Set, delete, empty, fromList,
                                                insert, member, null, toList)
import qualified Data.Set                      (filter)
import           Data.Text                     (Text, pack)
import           Debug.Trace                   (trace)
import           Error                         (throwMaybeM)
import           Model.Actions.Results         (AccessRes (CompleteAR, SimpleAR),
                                                AcquisitionRes (Complete, Simple),
                                                CompleteAccessRes (CompleteAccessRes),
                                                CompleteAcquisitionRes (CompleteAcquisitionRes),
                                                SimpleAccessRes (SimpleAccessRes),
                                                SimpleAcquisitionRes (SimpleAcquisitionRes))
import           Model.Core                    (AcquisitionActionF,
                                                ActionEffectKey (ObjectKey),
                                                ActionManagement (AVManagementKey, CAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SAConManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ConsumptionActionF (_consumptionAction),
                                                ContainerAccessActionF,
                                                GameComputation,
                                                GameState (_narration, _player, _world),
                                                ImplicitStimulusActionF,
                                                Location (_locationActionManagement),
                                                Narration (Narration, _actionConsequence),
                                                Object (_description, _descriptives, _objectActionManagement),
                                                Player (_location, _playerActions),
                                                PosturalActionF,
                                                SpatialRelationship (Inventory),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                                _objectSemanticMap)
import           Model.Core.Mappings           (GIDToDataMap, _getGIDToDataMap)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Container, Surface)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SimpleAccessVerb)
import           Model.Parser.Composites.Nouns (ConsumableNounPhrase (ConsumableNounPhrase),
                                                ContainerPhrase (ContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                InstrumentalAccessNounPhrase (InstrumentalAccessNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                ObjectPhrase (ObjectPhrase),
                                                SupportPhrase (ContainerSupport, SurfaceSupport),
                                                SurfacePhrase (SimpleSurfacePhrase, SurfacePhrase))
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase),
                                                ContainerAccessVerbPhrase (ContainerAccessVerbPhrase, SimpleAccessContainerVerbPhrase))
import           Model.Parser.GCase            (NounKey (ConsumableNounKey, ContainerKey, DirectionalStimulusKey, InstrumentAccessNounKey, ObjectiveKey, SurfaceKey))


getDescriptionM :: GID Object -> GameComputation Identity Text
getDescriptionM oid = do
  _description <$> getObjectM oid


parseAcquisitionPhrase :: AcquisitionVerbPhrase -> AcquisitionRes
parseAcquisitionPhrase avp =  case avp of
  SimpleAcquisitionVerbPhrase _ ophrase ->
    let okey = parseObjectPhrase ophrase
    in Simple $ SimpleAcquisitionRes okey ophrase
  (AcquisitionVerbPhrase _ ophrase _ sphrase) ->
    let okey = parseObjectPhrase ophrase
        skey = parseSupportPhrase sphrase
    in Complete $ CompleteAcquisitionRes okey ophrase skey sphrase

parseConsumptionPhrase :: ConsumptionVerbPhrase -> (ConsumableNounPhrase,NounKey)
parseConsumptionPhrase (ConsumptionVerbPhrase _ ophrase) = Data.Bifunctor.second ConsumableNounKey $
    let obj = case ophrase of
               (ConsumableNounPhrase (SimpleNounPhrase obj'))             -> obj'
               (ConsumableNounPhrase (NounPhrase _ obj'))                 -> obj'
               (ConsumableNounPhrase (DescriptiveNounPhrase _ obj'))      -> obj'
               (ConsumableNounPhrase (DescriptiveNounPhraseDet _ _ obj')) -> obj'
    in (ophrase, obj)

parseAccessPhrase :: ContainerAccessVerbPhrase -> AccessRes
parseAccessPhrase cavp = case cavp of
  SimpleAccessContainerVerbPhrase _ cphrase ->
    let ckey = parseContainerPhrase cphrase
    in SimpleAR $ SimpleAccessRes ckey cphrase
  ContainerAccessVerbPhrase _ cphrase iphrase ->
    let ckey = parseContainerPhrase cphrase
        ikey = parseInstrumentalAccessNounPhrase iphrase
    in CompleteAR $ CompleteAccessRes ckey cphrase ikey iphrase

parseDirectionalStimulusNounPhrase :: DirectionalStimulusNounPhrase -> NounKey
parseDirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase _ nounPhrase) =
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

parseInstrumentalAccessNounPhrase :: InstrumentalAccessNounPhrase -> NounKey
parseInstrumentalAccessNounPhrase (InstrumentalAccessNounPhrase _ nounPhrase) =
  let noun = case nounPhrase of
               SimpleNounPhrase instrument             -> instrument
               NounPhrase _ instrument                 -> instrument
               DescriptiveNounPhrase _ instrument      -> instrument
               DescriptiveNounPhraseDet _ _ instrument -> instrument
  in InstrumentAccessNounKey noun

parseConsumablePhrase :: ConsumableNounPhrase -> NounKey
parseConsumablePhrase (ConsumableNounPhrase phrase) =
  let noun = case phrase of
                     SimpleNounPhrase noun'             -> noun'
                     NounPhrase _ noun'                 -> noun'
                     DescriptiveNounPhrase _ noun'      -> noun'
                     DescriptiveNounPhraseDet _ _ noun' -> noun'
  in ConsumableNounKey noun

parseContainerPhrase :: ContainerPhrase -> NounKey
parseContainerPhrase (ContainerPhrase nounPhrase) =
  let noun = case nounPhrase of
               SimpleNounPhrase container             -> container
               NounPhrase _ container                 -> container
               DescriptiveNounPhrase _ container      -> container
               DescriptiveNounPhraseDet _ _ container -> container
  in ContainerKey noun

parseSupportPhrase :: SupportPhrase -> NounKey
parseSupportPhrase supportPhrase = case supportPhrase of
  SurfaceSupport surfacePhrase ->
    case surfacePhrase of
      SimpleSurfacePhrase nounPhrase -> extractSurfaceNoun nounPhrase
      SurfacePhrase _ nounPhrase     -> extractSurfaceNoun nounPhrase
  ContainerSupport (ContainerPhrase nounPhrase) -> extractContainerNoun nounPhrase
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

processAcquisitionEffect :: AcquisitionVerb
                         -> GID AcquisitionActionF
                         -> GameComputation Identity ()
processAcquisitionEffect avp newActionGID = do
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing acquisition action for this phrase
        filteredActions = Data.Set.filter (\case AVManagementKey p _ -> p /= avp; _ -> True) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (AVManagementKey avp newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }

processSimpleContainerAccessEffect :: SimpleAccessVerb
                         -> GID ContainerAccessActionF
                         -> GameComputation Identity ()
processSimpleContainerAccessEffect sav newActionGID = do
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing acquisition action for this phrase
        filteredActions = Data.Set.filter (\case SAConManagementKey p _ -> p /= sav; _ -> True) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (SAConManagementKey sav newActionGID) filteredActions
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
        -- Extract just the verb from the phrase
        verb = case cvp of
          ConsumptionVerbPhrase v _ -> v
        -- Remove any existing consumption action for this verb
        filteredActions = Data.Set.filter (\case CAManagementKey v _ -> v /= verb; _ -> True) playerActionSet
        -- Add the new action with just the verb
        updatedActions = Data.Set.insert (CAManagementKey verb newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }

processPositivePosturalEffect :: PositivePosturalVerb
                               -> GID PosturalActionF
                               -> GameComputation Identity ()
processPositivePosturalEffect verb newActionGID = do
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing positive postural action for this verb
        filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }

processNegativePosturalEffect :: NegativePosturalVerb
                               -> GID PosturalActionF
                               -> GameComputation Identity ()
processNegativePosturalEffect verb newActionGID = do
  modify' $ \gs ->
    let player = gs._player
        ActionManagementFunctions playerActionSet = _playerActions player
        -- Remove any existing negative postural action for this verb
        filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) playerActionSet
        -- Add the new action
        updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
        updatedPlayerActions = ActionManagementFunctions updatedActions
        updatedPlayer = player { _playerActions = updatedPlayerActions }
    in gs { _player = updatedPlayer }

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

modifyObjectM :: GID Object
              -> (Object -> Object)
              -> GameComputation Identity ()
modifyObjectM oid objectF = do
  world <- gets _world
  let objectMap = _getGIDToDataMap $ _objectMap world
  object <- throwMaybeM ("Object not found: " <> pack (show oid)) $
            Data.Map.Strict.lookup oid objectMap
  let updatedObject = objectF object
  let updatedObjectMap = Data.Map.Strict.insert oid updatedObject objectMap
      updatedWorld = world { _objectMap = (_objectMap world) { _getGIDToDataMap = updatedObjectMap } }
  modify' (\gs -> gs { _world = updatedWorld })

modifyPlayerM :: (Player -> Player)
              -> GameComputation Identity ()
modifyPlayerM playerF = do
  currentPlayer <- getPlayerM
  let updatedPlayer = playerF currentPlayer
  modify' (\gs -> gs { _player = updatedPlayer })

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

updateActionConsequence :: Text -> Narration -> Narration
updateActionConsequence consequence narration =
  narration { _actionConsequence = consequence : _actionConsequence narration }
-- Add object to inventory (replaces current inventory insertion)
addToInventoryM :: GID Object -> GameComputation Identity ()
addToInventoryM oid = do
  modifySpatialRelationshipsForObjectM oid (Data.Set.insert Inventory)

-- Remove object from inventory
removeFromInventoryM :: GID Object -> GameComputation Identity ()
removeFromInventoryM oid = do
  modifySpatialRelationshipsForObjectM oid (Data.Set.delete Inventory)
