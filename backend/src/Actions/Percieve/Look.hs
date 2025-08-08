{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Percieve.Look ( lookAt
                             , dsvActionEnabled
                             , isvActionEnabled
                             , agentCanSee
                             , agentCannotSee
                             , manageImplicitStimulusProcess
                             , manageDirectionalStimulusProcess) where

import           Control.Monad                                           (filterM)
import           Control.Monad.Identity                                  (Identity)
import           Control.Monad.Reader.Class                              (asks)
import           Control.Monad.State                                     (gets)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import           Data.Set                                                (Set)
import qualified Data.Set
import           Data.Text                                               (Text)
import           GameState                                               (getActionManagementM,
                                                                          getObjectM,
                                                                          getPlayerActionsM,
                                                                          getPlayerM,
                                                                          modifyNarration)
import qualified GameState.Spatial                                       as Spatial
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Location                                                (getLocationM,
                                                                          getPlayerLocationM)
import           Model.GameState                                         (ActionManagement (_directionalStimulusActionManagement, _implicitStimulusActionManagement),
                                                                          ActionMaps (_directionalStimulusActionMap, _implicitStimulusActionMap),
                                                                          Config (_actionMaps),
                                                                          DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                                          GameComputation,
                                                                          GameState (_world),
                                                                          ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                          ImplicitStimulusActionMap,
                                                                          Location (_locationActionManagement, _objectSemanticMap, _title),
                                                                          Object (_descriptives, _objectActionManagement),
                                                                          Player (_inventory, _location),
                                                                          PlayerActions (_directionalStimulusActions, _implicitStimulusActions),
                                                                          SpatialRelationship (Contains, Supports),
                                                                          SpatialRelationshipMap (SpatialRelationshipMap),
                                                                          World (_spatialRelationshipMap),
                                                                          updateActionConsequence)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase                                      (NounKey (DirectionalStimulusKey))
import           Relude.String.Conversion                                (ToText (toText))

agentCanSee :: ImplicitStimulusActionF
agentCanSee = ImplicitStimulusActionF $ const (\loc -> modifyNarration $ updateActionConsequence ("You see: " <> toText (_title loc)))

agentCannotSee :: Text -> ImplicitStimulusActionF
agentCannotSee nosee = ImplicitStimulusActionF
  $ const (const (modifyNarration $ updateActionConsequence nosee))

lookAt :: DirectionalStimulusActionF
lookAt = DirectionalStimulusActionF lookAt'
  where
    lookAt' :: DirectionalStimulusNounPhrase -> GID Object -> GameComputation Identity ()
    lookAt' dsnp oid = do
          dsActionMap <- _directionalStimulusActionManagement <$> getActionManagementM oid
          case Data.Map.Strict.lookup look dsActionMap of
            Nothing -> modifyNarration $ updateActionConsequence "Programmer made a thing you can't look at"
            Just dsaGID -> do
              dsActionMap' <- asks (_directionalStimulusActionMap . _actionMaps)
              case Data.Map.Strict.lookup dsaGID dsActionMap' of
                Nothing -> modifyNarration $ updateActionConsequence "Programmer made a key to an action that can't be found"
                Just (DirectionalStimulusActionF actionFunc) -> actionFunc dsnp oid

isvActionEnabled :: ImplicitStimulusVerb -> ImplicitStimulusActionF
isvActionEnabled isv = ImplicitStimulusActionF actionEnabled
  where
    actionEnabled player loc = do
      let actionMap = _implicitStimulusActionManagement $ _locationActionManagement loc
      case Data.Map.Strict.lookup isv actionMap of
        Nothing -> error "Programmer Error: No implicit stimulus action found for verb: "
        Just (actionGID :: GID ImplicitStimulusActionF) -> do
          actionMap' ::  Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No implicit stimulus action found for verb: "
            Just (ImplicitStimulusActionF actionFunc) -> actionFunc player loc

dsvActionEnabled :: DirectionalStimulusVerb ->  DirectionalStimulusActionF
dsvActionEnabled dsv = DirectionalStimulusActionF actionEnabled
  where
    actionEnabled dsnp oid = do
      actionMap <- _directionalStimulusActionManagement .  _objectActionManagement <$> getObjectM oid
      case Data.Map.Strict.lookup dsv actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
        Just (actionGID :: GID DirectionalStimulusActionF) -> do
          actionMap' :: Map (GID DirectionalStimulusActionF) DirectionalStimulusActionF <- asks (_directionalStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
            Just (DirectionalStimulusActionF actionFunc) -> actionFunc dsnp oid

manageImplicitStimulusProcess :: ImplicitStimulusVerb -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  availableActions <- _implicitStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup isv availableActions of
    Nothing -> error "Programmer Error: No implicit stimulus action found for verb: "
    Just (actionGID :: GID ImplicitStimulusActionF) -> do
      actionMap :: ImplicitStimulusActionMap <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No implicit stimulus action found for GID: "
        Just (ImplicitStimulusActionF actionFunc) -> do
          player <- getPlayerM
          let lid = player._location
          loc <- getLocationM lid
          actionFunc player loc

manageDirectionalStimulusProcess :: DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ()
manageDirectionalStimulusProcess dsv dsnp = do
  availableActions <- _directionalStimulusActions <$> getPlayerActionsM
  case Data.Map.Strict.lookup dsv availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just (actionGID :: GID DirectionalStimulusActionF) -> do
      actionMap <- asks (_directionalStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just actionFunc -> do
          location <- getPlayerLocationM
          lookableWithSpatialAwareness actionFunc dsnp location


-- NEW: Enhanced lookable function with spatial awareness
lookableWithSpatialAwareness :: DirectionalStimulusActionF
                             -> DirectionalStimulusNounPhrase
                             -> Location
                             -> GameComputation Identity ()
lookableWithSpatialAwareness (DirectionalStimulusActionF actionF) dsnp@(DirectionalStimulusNounPhrase np) loc = do
  let nounKey = DirectionalStimulusKey dsn'
      dsn' = case np of
             SimpleNounPhrase dsn             -> dsn
             NounPhrase _ dsn                 -> dsn
             DescriptiveNounPhrase  _ dsn     -> dsn
             DescriptiveNounPhraseDet _ _ dsn -> dsn

  -- First try direct location lookup (existing behavior)
  case Data.Map.Strict.lookup nounKey (_objectSemanticMap loc) of
    Just objGIDSet | not (Data.Set.null objGIDSet) -> do
      let firstObjGID = Data.Set.elemAt 0 objGIDSet
      actionF dsnp firstObjGID
    _ -> do
      -- NEW: If not found in location, search spatially
      Spatial.findObjectInInventoryContainers nounKey >>= \case
        Just objGID -> actionF dsnp objGID
        Nothing -> modifyNarration $ updateActionConsequence "That's not here. Try something else."



-- NEW: Spatial object finder
findObjectSpatially :: NounKey -> GameComputation Identity (Maybe (GID Object))
findObjectSpatially nounKey = do
  player <- getPlayerM
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

  -- Get all objects in player's inventory
  let inventoryObjects = concatMap Data.Set.toList $ Data.Map.Strict.elems (_inventory player)

  -- For each inventory object, check what it contains/supports
  findInContainers nounKey inventoryObjects spatialMap

-- NEW: Search within containers/supporters in inventory
findInContainers :: NounKey
                 -> [GID Object]
                 -> Map (GID Object) (Set SpatialRelationship)
                 -> GameComputation Identity (Maybe (GID Object))
findInContainers nounKey inventoryObjects spatialMap = do
  -- For each object in inventory, get what it contains/supports
  containedObjects <- mapM (getContainedObjects spatialMap) inventoryObjects
  let allContained = concat containedObjects

  -- Check if any contained object matches our search
  findMatchingObject nounKey allContained

-- NEW: Get objects contained/supported by a given object
getContainedObjects :: Map (GID Object) (Set SpatialRelationship)
                    -> GID Object
                    -> GameComputation Identity [GID Object]
getContainedObjects spatialMap containerOID = case Data.Map.Strict.lookup containerOID spatialMap of
  Nothing -> pure []
  Just relationships -> do
    let containedGIDs = concatMap extractContained (Data.Set.toList relationships)
    pure containedGIDs
  where
    extractContained :: SpatialRelationship -> [GID Object]
    extractContained (Contains oidSet) = Data.Set.toList oidSet
    extractContained (Supports oidSet) = Data.Set.toList oidSet
    extractContained _                 = []

-- NEW: Check if any object matches the noun key
findMatchingObject :: NounKey -> [GID Object] -> GameComputation Identity (Maybe (GID Object))
findMatchingObject nounKey objectGIDs = do
  -- For each object, check if it matches the noun key by examining its descriptives
  matchingObjects <- filterM (objectMatchesNounKey nounKey) objectGIDs
  case matchingObjects of
    (firstMatch:_) -> pure (Just firstMatch)
    []             -> pure Nothing

-- NEW: Check if an object matches a noun key
objectMatchesNounKey :: NounKey -> GID Object -> GameComputation Identity Bool
objectMatchesNounKey nounKey oid = do
  obj <- getObjectM oid
  let descriptives = _descriptives obj

  -- Check if any of the object's descriptives match the noun key
  pure $ any (descriptiveMatchesNounKey nounKey) (Data.Set.toList descriptives)

-- NEW: Check if a descriptive phrase matches a noun key
descriptiveMatchesNounKey :: NounKey -> DirectionalStimulusNounPhrase -> Bool
descriptiveMatchesNounKey (DirectionalStimulusKey targetNoun) (DirectionalStimulusNounPhrase np) =
  let nounFromPhrase = case np of
        SimpleNounPhrase dsn             -> dsn
        NounPhrase _ dsn                 -> dsn
        DescriptiveNounPhrase _ dsn      -> dsn
        DescriptiveNounPhraseDet _ _ dsn -> dsn
  in targetNoun == nounFromPhrase
descriptiveMatchesNounKey _ _ = False
