module GameState.Spatial (findObjectInInventoryContainers
                         , getContainedObjects
                         , getSupportedObjects
                         , getAllAccessibleObjects
                         , getContainmentChain) where

import           Control.Monad                 (filterM)
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State.Class     (gets)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Set                      (Set)
import qualified Data.Set
import           GameState                     (getInventoryObjectsM,
                                                getObjectM, getPlayerM)
import           Model.GameState               (GameComputation,
                                                GameState (_world),
                                                Object (_descriptives),
                                                SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase),
                                                ObjectPhrase)
import           Model.Parser.GCase            (NounKey (DirectionalStimulusKey))

-- | Find an object matching a noun key in any containers/supporters in player's inventory

findObjectInInventoryContainers :: NounKey -> GameComputation Identity (Maybe (GID Object))
findObjectInInventoryContainers nounKey = do
  inventoryObjects <- getInventoryObjectsM
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
  accessibleObjects <- getAllAccessibleObjects inventoryObjects spatialMap
  findMatchingObject nounKey accessibleObjects

-- | Get all objects contained by a given object
getContainedObjects :: GID Object -> GameComputation Identity [GID Object]
getContainedObjects containerOID = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
  case Data.Map.Strict.lookup containerOID spatialMap of
    Nothing -> pure []
    Just relationships -> do
      let containedGIDs = concatMap extractContained (Data.Set.toList relationships)
      pure containedGIDs
  where
    extractContained :: SpatialRelationship -> [GID Object]
    extractContained (Contains oidSet) = Data.Set.toList oidSet
    extractContained _                 = []

-- | Get all objects supported by a given object
getSupportedObjects :: GID Object -> GameComputation Identity [GID Object]
getSupportedObjects supporterOID = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
  case Data.Map.Strict.lookup supporterOID spatialMap of
    Nothing -> pure []
    Just relationships -> do
      let supportedGIDs = concatMap extractSupported (Data.Set.toList relationships)
      pure supportedGIDs
  where
    extractSupported :: SpatialRelationship -> [GID Object]
    extractSupported (Supports oidSet) = Data.Set.toList oidSet
    extractSupported _                 = []

-- | Get all objects accessible from a list of starting objects (recursive containment/support)
getAllAccessibleObjects :: [GID Object]
                        -> Map (GID Object) (Set SpatialRelationship)
                        -> GameComputation Identity [GID Object]
getAllAccessibleObjects startingObjects spatialMap = do
  -- Use breadth-first search to find all accessible objects
  go startingObjects []
  where
    go :: [GID Object] -> [GID Object] -> GameComputation Identity [GID Object]
    go [] acc = pure acc
    go (current:remaining) acc = do
      if current `elem` acc
        then go remaining acc  -- Avoid cycles
        else do
          -- Get objects contained/supported by current object
          contained <- getObjectRelatives current spatialMap
          go (remaining ++ contained) (current:acc)

    getObjectRelatives :: GID Object
                       -> Map (GID Object) (Set SpatialRelationship)
                       -> GameComputation Identity [GID Object]
    getObjectRelatives oid spatialMap' = do
      case Data.Map.Strict.lookup oid spatialMap' of
        Nothing -> pure []
        Just relationships -> do
          let relatives = concatMap extractRelatives (Data.Set.toList relationships)
          pure relatives

    extractRelatives :: SpatialRelationship -> [GID Object]
    extractRelatives (Contains oidSet) = Data.Set.toList oidSet
    extractRelatives (Supports oidSet) = Data.Set.toList oidSet
    extractRelatives _                 = []

-- | Get the containment chain for an object (what contains what, up to the root)
getContainmentChain :: GID Object -> GameComputation Identity [GID Object]
getContainmentChain targetOID = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
  buildChain targetOID spatialMap []
  where
    buildChain :: GID Object
               -> Map (GID Object) (Set SpatialRelationship)
               -> [GID Object]
               -> GameComputation Identity [GID Object]
    buildChain oid spatialMap acc = do
      if oid `elem` acc
        then pure acc  -- Avoid cycles
        else do
          case Data.Map.Strict.lookup oid spatialMap of
            Nothing -> pure (oid:acc)
            Just relationships -> do
              let containers = getContainers relationships
              case containers of
                []               -> pure (oid:acc)  -- No container, we're at the root
                (containerOID:_) -> buildChain containerOID spatialMap (oid:acc)

    getContainers :: Set SpatialRelationship -> [GID Object]
    getContainers relationships =
      [containerOID | ContainedIn containerOID <- Data.Set.toList relationships] ++
      [supporterOID | SupportedBy supporterOID <- Data.Set.toList relationships]

-- | Helper: Check if any object matches a noun key
findMatchingObject :: NounKey -> [GID Object] -> GameComputation Identity (Maybe (GID Object))
findMatchingObject nounKey objectGIDs = do
  matchingObjects <- filterM (objectMatchesNounKey nounKey) objectGIDs
  case matchingObjects of
    (firstMatch:_) -> pure (Just firstMatch)
    []             -> pure Nothing

-- | Helper: Check if an object matches a noun key
objectMatchesNounKey :: NounKey -> GID Object -> GameComputation Identity Bool
objectMatchesNounKey nounKey oid = do
  obj <- getObjectM oid
  let descriptives = _descriptives obj
  pure $ any (descriptiveMatchesNounKey nounKey) (Data.Set.toList descriptives)

-- | Helper: Check if a descriptive phrase matches a noun key
descriptiveMatchesNounKey :: NounKey -> DirectionalStimulusNounPhrase -> Bool
descriptiveMatchesNounKey (DirectionalStimulusKey targetNoun) (DirectionalStimulusNounPhrase _ np) =
  let nounFromPhrase = case np of
        SimpleNounPhrase dsn             -> dsn
        NounPhrase _ dsn                 -> dsn
        DescriptiveNounPhrase _ dsn      -> dsn
        DescriptiveNounPhraseDet _ _ dsn -> dsn
  in targetNoun == nounFromPhrase
descriptiveMatchesNounKey _ _ = False
