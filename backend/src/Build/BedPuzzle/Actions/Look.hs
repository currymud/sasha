 module Build.BedPuzzle.Actions.Look where
import           Control.Applicative    (Alternative ((<|>)))
import           Control.Monad          (filterM, unless)
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (gets)
import qualified Data.Map.Strict
import           Data.Maybe             (listToMaybe)
import qualified Data.Set
import           Data.Text              (Text, intercalate)
import           GameState              (getObjectM, modifyNarration)
import           GameState.Perception   (isObjectPerceivable)
import           Model.GameState        (DirectionalStimulusActionF (ObjectDirectionalStimulusActionF),
                                         DirectionalStimulusContainerActionF (ObjectDirectionalStimulusContainerActionF),
                                         GameComputation, GameState (_world),
                                         Object,
                                         SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                         SpatialRelationshipMap (SpatialRelationshipMap),
                                         World (_spatialRelationshipMap),
                                         _shortName, updateActionConsequence)
import           Model.GID              (GID)


findContainer :: Data.Set.Set SpatialRelationship -> Maybe (GID Object)
findContainer relationships =
      listToMaybe [cid | ContainedIn cid <- Data.Set.toList relationships] <|>
      listToMaybe [sid | SupportedBy sid <- Data.Set.toList relationships]

lookInF :: GID Object -> Text -> DirectionalStimulusContainerActionF
lookInF containerGID flavorText = ObjectDirectionalStimulusContainerActionF lookInAction
  where
    lookInAction :: GameComputation Identity ()
    lookInAction = do
      -- First add the flavor text
      modifyNarration $ updateActionConsequence flavorText

      -- Get spatial relationships once
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

      -- Pass spatialMap to avoid duplicate lookup
      let containedObjects = getContainedObjects containerGID spatialMap

      if null containedObjects
        then modifyNarration $ updateActionConsequence "It's empty."
        else do
          -- Get descriptions of contained objects
          descriptions <- mapM getObjectDescription containedObjects
          let contentText = "You see: " <> Data.Text.intercalate ", " descriptions
          modifyNarration $ updateActionConsequence contentText

    getObjectDescription objGID = do
      obj <- getObjectM objGID
      pure $ _shortName obj

lookAtF :: GID Object -> DirectionalStimulusActionF
lookAtF objGID = ObjectDirectionalStimulusActionF lookAction
  where
    lookAction = do
      -- Get object info
      obj <- getObjectM objGID
      -- Get spatial relationships
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

      -- Let generateLocationNarration handle the case deconstruction
      generateLocationNarration obj objGID spatialMap
      -- Also check what's on/in this object
      generateContentsNarration objGID spatialMap

-- Updated to handle the case deconstruction internally
generateLocationNarration :: Object
                          -> GID Object
                          -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                          -> GameComputation Identity ()
generateLocationNarration obj objGID spatialMap =
  case Data.Map.Strict.lookup objGID spatialMap of
    Just relationships
      | Inventory `Data.Set.member` relationships ->
          modifyNarration $ updateActionConsequence $ "You're holding the " <> _shortName obj
      | Just containerGID <- findContainer relationships -> do
          container <- getObjectM containerGID
          modifyNarration $ updateActionConsequence $ "The " <> _shortName obj <> " is in/on the " <> _shortName container
      | otherwise ->
          modifyNarration $ updateActionConsequence $ "You see the " <> _shortName obj
    Nothing ->
      modifyNarration $ updateActionConsequence $ "You see the " <> _shortName obj

generateContentsNarration :: GID Object
                          -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                          -> GameComputation Identity ()
generateContentsNarration objGID spatialMap = do
  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> error $ "Programmer Error: Object " <> show objGID <> " not found in SpatialRelationshipMap"
    Just objRelationships -> do
      -- Use objRelationships directly instead of calling helper functions
      let supportedObjects = [oid | Supports oidSet <- Data.Set.toList objRelationships,
                                    oid <- Data.Set.toList oidSet]
      let containedObjects = [oid | Contains oidSet <- Data.Set.toList objRelationships,
                                    oid <- Data.Set.toList oidSet]

      -- Filter to only perceivable objects
      perceivableSupportedObjects <- filterM isObjectPerceivable supportedObjects
      perceivableContainedObjects <- filterM isObjectPerceivable containedObjects

      -- Generate narration for supported objects (on the object)
      unless (null perceivableSupportedObjects) $ do
        supportedNames <- mapM (fmap _shortName . getObjectM) perceivableSupportedObjects
        let onText = "On it you see: " <> Data.Text.intercalate ", " supportedNames
        modifyNarration $ updateActionConsequence onText

      -- Generate narration for contained objects (in the object)
      unless (null perceivableContainedObjects) $ do
        containedNames <- mapM (fmap _shortName . getObjectM) perceivableContainedObjects
        let inText = "In it you see: " <> Data.Text.intercalate ", " containedNames
        modifyNarration $ updateActionConsequence inText
-- Helper function for supported objects (similar to getContainedObjects)
getSupportedObjects :: GID Object
                    -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                    -> [GID Object]
getSupportedObjects objGID spatialMap =
  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> []
    Just relationships ->
      [oid | Supports oidSet <- Data.Set.toList relationships,
             oid <- Data.Set.toList oidSet]

-- Updated getContainedObjects signature
getContainedObjects :: GID Object
                    -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                    -> [GID Object]
getContainedObjects objGID spatialMap =
  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> []
    Just relationships ->
      [oid | Contains oidSet <- Data.Set.toList relationships,
             oid <- Data.Set.toList oidSet]
