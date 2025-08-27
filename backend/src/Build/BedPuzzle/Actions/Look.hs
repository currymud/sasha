module Build.BedPuzzle.Actions.Look (lookAtF,lookInF) where
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (gets)
import qualified Data.Map.Strict
import           Data.Set               (Set)
import qualified Data.Set
import           Data.Text              (Text, intercalate)
import           GameState              (getObjectM, modifyNarration)
import           GameState.Spatial      (getContainmentChain)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         DirectionalStimulusContainerActionF (DirectionalStimulusContainerActionF),
                                         GameComputation, GameState (_world),
                                         Object (_description, _shortName),
                                         SpatialRelationship (ContainedIn, Contains),
                                         SpatialRelationshipMap (SpatialRelationshipMap),
                                         World (_spatialRelationshipMap),
                                         updateActionConsequence)
import           Model.GID              (GID)

lookAtF :: GID Object -> DirectionalStimulusActionF
lookAtF oid = DirectionalStimulusActionF (const (const lookAt'))
  where
    lookAt' :: GameComputation Identity ()
    lookAt' = do
      obj <- getObjectM oid
      locationText <- getObjectLocationText oid
      let fullDescription = _description obj <> locationText
      modifyNarration $ updateActionConsequence fullDescription

lookInF :: GID Object -> Text -> DirectionalStimulusContainerActionF
lookInF objGID flavorText = DirectionalStimulusContainerActionF (const containerLook')
  where
    containerLook' :: GameComputation Identity ()
    containerLook' = do
      obj <- getObjectM objGID
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

      case Data.Map.Strict.lookup objGID spatialMap of
        Nothing -> do
          -- Object has no spatial relationships, show base description + flavor text
          let msg = _description obj <> " " <> flavorText
          modifyNarration $ updateActionConsequence msg
        Just relationships -> do
          containedObjects <- getContainedObjectNames objGID relationships

          -- Build dynamic description
          let baseDescription = _description obj
              fullDescription = buildFullDescription baseDescription flavorText containedObjects

          modifyNarration $ updateActionConsequence fullDescription

-- | Extract contained objects and get their short names
getContainedObjectNames :: GID Object
                             -> Set SpatialRelationship
                             -> GameComputation Identity [Text]
getContainedObjectNames _objGID relationships = do
  let containedGIDs = concatMap extractContained (Data.Set.toList relationships)
  mapM getObjectShortName containedGIDs
  where
    extractContained :: SpatialRelationship -> [GID Object]
    extractContained (Contains oidSet) = Data.Set.toList oidSet
    extractContained _                 = []

-- | Get the short name of an object
getObjectShortName :: GID Object -> GameComputation Identity Text
getObjectShortName oid = do
  obj <- getObjectM oid
  pure $ _shortName obj

-- | Build the full description with supported/contained objects and flavor text
buildFullDescription :: Text -> Text -> [Text] -> Text
buildFullDescription baseDescription flavorText containedObjects =

  let containText = case containedObjects of
        []     -> ""
        [item] -> " In the " <> baseDescription <> " is " <> item <> "."
        items  -> " In the " <> baseDescription <> " are " <> Data.Text.intercalate ", " items <> "."

  in baseDescription <> " " <> flavorText <> containText

getObjectLocationText :: GID Object -> GameComputation Identity Text
getObjectLocationText oid = do
  chain <- getContainmentChain oid
  case chain of
    [_] -> pure ""  -- Object is at root level
    (objGID:parentGID:_) -> do
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      case Data.Map.Strict.lookup objGID spatialMap of
        Nothing -> pure ""
        Just relationships -> do
          parentObj <- getObjectM parentGID
          let isContained = any (\case ContainedIn pid -> pid == parentGID; _ -> False) (Data.Set.toList relationships)
          pure $ if isContained
                 then ". It's in the " <> _shortName parentObj
                 else ". It's on the " <> _shortName parentObj
    [] -> pure ""
