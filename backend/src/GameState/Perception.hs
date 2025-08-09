module GameState.Perception (computePerceptionMapFromSpatial, rebuildPerceptionMapFromSpatial) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (gets)
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           GameState                     (getObjectM,
                                                modifyPerceptionMapM)
import           Model.GameState               (GameComputation,
                                                GameState (_world),
                                                Object (_descriptives),
                                                SpatialRelationship (ContainedIn, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase)

-- | Compute perception map from spatial relationships
-- Objects are perceivable if they are:
-- 1. In inventory
-- 2. Surface-level (not contained/supported by others)
-- 3. Supported by perceivable objects
computePerceptionMapFromSpatial :: SpatialRelationshipMap
                                -> GameComputation Identity (Map.Map DirectionalStimulusNounPhrase (Set.Set (GID Object)))
computePerceptionMapFromSpatial (SpatialRelationshipMap spatialMap) = do
  let perceivableGIDs = findPerceivableGIDs spatialMap
  buildPerceptionMapFromGIDs perceivableGIDs

-- | Find perceivable object GIDs from spatial relationships
findPerceivableGIDs :: Map.Map (GID Object) (Set.Set SpatialRelationship) -> [GID Object]
findPerceivableGIDs spatialMap =
  let inventoryGIDs = [oid | (oid, rels) <- Map.toList spatialMap, Inventory `Set.member` rels]
      surfaceGIDs = [oid | (oid, rels) <- Map.toList spatialMap,
                          not (any isContainedOrSupported (Set.toList rels))]
      supportedGIDs = getSupportedGIDs spatialMap (inventoryGIDs ++ surfaceGIDs)
  in inventoryGIDs ++ surfaceGIDs ++ supportedGIDs
  where
    isContainedOrSupported (ContainedIn _) = True
    isContainedOrSupported (SupportedBy _) = True
    isContainedOrSupported _               = False

-- | Get GIDs of objects supported by perceivable objects
getSupportedGIDs :: Map.Map (GID Object) (Set.Set SpatialRelationship) -> [GID Object] -> [GID Object]
getSupportedGIDs spatialMap perceivableGIDs =
  let directSupported = concatMap (getDirectlySupported spatialMap) perceivableGIDs
      newGIDs = filter (`notElem` perceivableGIDs) directSupported
  in if null newGIDs
     then []
     else directSupported ++ getSupportedGIDs spatialMap (perceivableGIDs ++ newGIDs)
  where
    getDirectlySupported smap oid =
      case Map.lookup oid smap of
        Nothing   -> []
        Just rels -> concatMap extractSupported (Set.toList rels)

    extractSupported (Supports gidSet) = Set.toList gidSet
    extractSupported _                 = []

-- | Build perception map from GIDs by looking up their descriptives
buildPerceptionMapFromGIDs :: [GID Object]
                           -> GameComputation Identity (Map.Map DirectionalStimulusNounPhrase (Set.Set (GID Object)))
buildPerceptionMapFromGIDs gids = do
  perceptionEntries <- mapM addGIDToPerception gids
  pure $ Map.fromListWith Set.union (concat perceptionEntries)
  where
    addGIDToPerception gid = do
      obj <- getObjectM gid
      let descriptives = Set.toList (_descriptives obj)
      pure [(desc, Set.singleton gid) | desc <- descriptives]

-- | Rebuild perception map in game state
rebuildPerceptionMapFromSpatial :: GameComputation Identity ()
rebuildPerceptionMapFromSpatial = do
  spatialMap <- gets (_spatialRelationshipMap . _world)
  newPerceptionMap <- computePerceptionMapFromSpatial spatialMap
  modifyPerceptionMapM (const newPerceptionMap)
