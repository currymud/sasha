module GameState.Perception  where

import           Control.Monad                 (filterM)
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (gets, modify')
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           GameState                     (getObjectM)
import           Model.GameState               (GameComputation,
                                                GameState (_world),
                                                Object (_descriptives),
                                                SpatialRelationship (ContainedIn, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_perceptionMap, _spatialRelationshipMap))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase)


-- | Perception Map Contract Definition
-- The perception map is the single source of truth for object perception

-- | Contract: Query the perception map for objects matching a phrase
queryPerceptionMap :: DirectionalStimulusNounPhrase
                   -> GameComputation Identity (Set.Set (GID Object))
queryPerceptionMap phrase = do
  perceptionMap <- gets (_perceptionMap . _world)
  pure $ Map.findWithDefault Set.empty phrase perceptionMap

-- | Contract: Check if an object is currently perceivable
isObjectPerceivable :: GID Object -> GameComputation Identity Bool
isObjectPerceivable oid = do
  obj <- getObjectM oid
  let descriptives = Set.toList (_descriptives obj)
  -- Object is perceivable if it appears under any of its descriptive phrases
  anyPerceivable <- mapM queryPerceptionMap descriptives
  pure $ any (Set.member oid) anyPerceivable

-- | Contract: Get all currently perceivable objects
getAllPerceivableObjects :: GameComputation Identity (Set.Set (GID Object))
getAllPerceivableObjects = do
  perceptionMap <- gets (_perceptionMap . _world)
  pure $ Set.unions (Map.elems perceptionMap)

-- | Compute which objects should be perceivable based on spatial relationships
computePerceivableObjects :: GameComputation Identity (Set.Set (GID Object))
computePerceivableObjects = do
  spatialMap <- gets (_spatialRelationshipMap . _world)
  let SpatialRelationshipMap smap = spatialMap

  -- Objects in inventory
  let inventoryObjects = Set.fromList [oid | (oid, rels) <- Map.toList smap,
                                            Inventory `Set.member` rels]

  -- Surface-level objects (not contained or supported by others)
  let surfaceObjects = Set.fromList [oid | (oid, rels) <- Map.toList smap,
                                          not (any isContainedOrSupported (Set.toList rels))]

  -- Objects supported by perceivable objects (recursive)
  supportedObjects <- getSupportedObjects (Set.union inventoryObjects surfaceObjects) smap

  pure $ Set.unions [inventoryObjects, surfaceObjects, supportedObjects]
  where
    isContainedOrSupported (ContainedIn _) = True
    isContainedOrSupported (SupportedBy _) = True
    isContainedOrSupported _               = False

-- | Get objects supported by perceivable objects (recursive)
getSupportedObjects :: Set.Set (GID Object)
                    -> Map.Map (GID Object) (Set.Set SpatialRelationship)
                    -> GameComputation Identity (Set.Set (GID Object))
getSupportedObjects perceivableObjects spatialMap = do
  let directSupported = Set.unions [getSupported oid | oid <- Set.toList perceivableObjects]
      newObjects = Set.difference directSupported perceivableObjects

  if Set.null newObjects
    then pure directSupported
    else getSupportedObjects (Set.union perceivableObjects newObjects) spatialMap
  where
    getSupported oid =
      case Map.lookup oid spatialMap of
        Nothing   -> Set.empty
        Just rels -> Set.unions [objSet | Supports objSet <- Set.toList rels]

-- | Validation errors for perception map contract
data PerceptionMapError
  = DanglingObjectReference (GID Object) DirectionalStimulusNounPhrase
  | MissingPerceivableObject (GID Object)
  | InconsistentDescriptives (GID Object) DirectionalStimulusNounPhrase
  deriving (Show, Eq)

-- | Validate that the perception map satisfies all contract guarantees
validatePerceptionMapContract :: GameComputation Identity [PerceptionMapError]
validatePerceptionMapContract = do
  perceptionMap <- gets (_perceptionMap . _world)

  -- Check completeness - all perceivable objects are in the map
  shouldBePerceivable <- computePerceivableObjects
  let actuallyPerceivable = Set.unions (Map.elems perceptionMap)
      missing = Set.difference shouldBePerceivable actuallyPerceivable

  -- Check descriptive consistency
  descriptiveErrors <- validateDescriptiveConsistency perceptionMap

  let completenessErrors = [MissingPerceivableObject oid | oid <- Set.toList missing]

  pure $ completenessErrors ++ descriptiveErrors

-- | Validate that objects appear under all their descriptive phrases
validateDescriptiveConsistency :: Map.Map DirectionalStimulusNounPhrase (Set.Set (GID Object))
                               -> GameComputation Identity [PerceptionMapError]
validateDescriptiveConsistency perceptionMap = do
  let allObjects = Set.unions (Map.elems perceptionMap)
  inconsistencies <- mapM checkObjectDescriptives (Set.toList allObjects)
  pure $ concat inconsistencies
  where
    checkObjectDescriptives oid = do
      obj <- getObjectM oid
      let descriptives = Set.toList (_descriptives obj)
      missingDescriptives <- filterM (fmap not . objectInPhrase oid) descriptives
      pure [InconsistentDescriptives oid phrase | phrase <- missingDescriptives]

    objectInPhrase oid phrase = do
      objects <- queryPerceptionMap phrase
      pure $ Set.member oid objects

-- | Add an object to the perception map under all its descriptive phrases
addObjectToPerceptionMap :: GID Object -> GameComputation Identity ()
addObjectToPerceptionMap oid = do
  obj <- getObjectM oid
  let descriptives = Set.toList (_descriptives obj)
  mapM_ (addObjectUnderPhrase oid) descriptives
  where
    addObjectUnderPhrase oid phrase =
      modifyPerceptionMapM $ \perceptionMap ->
        let currentObjects = Map.findWithDefault Set.empty phrase perceptionMap
            updatedObjects = Set.insert oid currentObjects
        in Map.insert phrase updatedObjects perceptionMap

-- | Remove an object from the perception map under all its descriptive phrases
removeObjectFromPerceptionMap :: GID Object -> GameComputation Identity ()
removeObjectFromPerceptionMap oid = do
  obj <- getObjectM oid
  let descriptives = Set.toList (_descriptives obj)
  mapM_ (removeObjectUnderPhrase oid) descriptives
  where
    removeObjectUnderPhrase oid phrase =
      modifyPerceptionMapM $ \perceptionMap ->
        let currentObjects = Map.findWithDefault Set.empty phrase perceptionMap
            updatedObjects = Set.delete oid currentObjects
        in if Set.null updatedObjects
           then Map.delete phrase perceptionMap
           else Map.insert phrase updatedObjects perceptionMap


-- | Build perception map from a list of object GIDs
buildPerceptionMapFromObjects :: [GID Object]
                              -> GameComputation Identity (Map.Map DirectionalStimulusNounPhrase (Set.Set (GID Object)))
buildPerceptionMapFromObjects objectGIDs = do
  perceptionEntries <- mapM getObjectPerceptionEntries objectGIDs
  pure $ Map.fromListWith Set.union (concat perceptionEntries)
  where
    getObjectPerceptionEntries oid = do
      obj <- getObjectM oid
      let descriptives = Set.toList (_descriptives obj)
      pure [(desc, Set.singleton oid) | desc <- descriptives]

modifyPerceptionMapM :: (Map.Map DirectionalStimulusNounPhrase (Set.Set (GID Object))
                      -> Map.Map DirectionalStimulusNounPhrase (Set.Set (GID Object)))
                     -> GameComputation Identity ()
modifyPerceptionMapM perceptionMapF = do
  world <- gets _world
  let currentPerceptionMap = _perceptionMap world
      updatedPerceptionMap = perceptionMapF currentPerceptionMap
      updatedWorld = world { _perceptionMap = updatedPerceptionMap }
  modify' (\gs -> gs { _world = updatedWorld })
