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
                                         DirectionalStimulusContainerActionF (DirectionalStimulusContainerActionF),
                                         GameComputation, GameState (_world),
                                         Object,
                                         SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                         SpatialRelationshipMap (SpatialRelationshipMap),
                                         World (_spatialRelationshipMap),
                                         _shortName, updateActionConsequence)
import           Model.GID              (GID)



lookAtF :: GID Object -> DirectionalStimulusActionF
lookAtF objGID = ObjectDirectionalStimulusActionF $ \_ _ -> lookAction
  where
    lookAction = do
      -- Get object info
      obj <- getObjectM objGID
      -- Get spatial relationships to determine location context
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      case Data.Map.Strict.lookup objGID spatialMap of
        Just relationships -> do
          generateLocationNarration obj relationships
          -- Also check what's on/in this object
          generateContentsNarration objGID relationships
        Nothing -> do
          modifyNarration $ updateActionConsequence $ "You see the " <> _shortName obj
          -- Still check for contents even if object has no spatial relationships
          generateContentsNarration objGID Data.Set.empty

generateLocationNarration :: Object
                               -> Data.Set.Set SpatialRelationship
                               -> GameComputation Identity ()
generateLocationNarration obj relationships
  | Inventory `Data.Set.member` relationships =
      modifyNarration $ updateActionConsequence $ "You're holding the " <> _shortName obj
  | Just containerGID <- findContainer relationships = do
      container <- getObjectM containerGID
      modifyNarration $ updateActionConsequence $ "The " <> _shortName obj <> " is in/on the " <> _shortName container
  | otherwise =
      modifyNarration $ updateActionConsequence $ "You see the " <> _shortName obj

generateContentsNarration :: GID Object
                                   -> GameComputation Identity ()
generateContentsNarration objGID = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

  -- Look up what this object contains/supports by checking its own relationships
  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> pure () -- Object has no spatial relationships
    Just objRelationships -> do
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

findContainer :: Data.Set.Set SpatialRelationship -> Maybe (GID Object)
findContainer relationships =
      listToMaybe [cid | ContainedIn cid <- Data.Set.toList relationships] <|>
      listToMaybe [sid | SupportedBy sid <- Data.Set.toList relationships]

lookInF :: GID Object -> Text -> DirectionalStimulusContainerActionF
lookInF containerGID flavorText = DirectionalStimulusContainerActionF lookInAction
  where
    lookInAction :: GID Object -> GameComputation Identity ()
    lookInAction containerObjGID = do
      -- First add the flavor text
      modifyNarration $ updateActionConsequence flavorText

      -- Get the contained objects
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      containedObjects <- getContainedObjects containerGID

      if null containedObjects
        then modifyNarration $ updateActionConsequence $ "It's empty."
        else do
          -- Get descriptions of contained objects
          descriptions <- mapM getObjectDescription containedObjects
          let contentText = "You see: " <> Data.Text.intercalate ", " descriptions
          modifyNarration $ updateActionConsequence contentText

    getObjectDescription objGID = do
      obj <- getObjectM objGID
      pure $ _shortName obj

getContainedObjects :: GID Object -> GameComputation Identity [GID Object]
getContainedObjects containerOID = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
  case Data.Map.Strict.lookup containerOID spatialMap of
    Nothing -> pure []
    Just relationships -> do
      let containedGIDs :: [GID Object]
          containedGIDs = [oid | Contains oidSet <- Data.Set.toList relationships,
                                 oid <- Data.Set.toList oidSet]
      pure containedGIDs
