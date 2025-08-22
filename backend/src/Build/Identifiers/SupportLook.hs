module Build.Identifiers.SupportLook (supportLookF) where

import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (gets)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GameState              (getObjectM, modifyNarration)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation,
                                         Object (_description, _shortName),
                                         SpatialRelationship (Contains, Supports),
                                         SpatialRelationshipMap (SpatialRelationshipMap),
                                         World (_spatialRelationshipMap),
                                         _world, updateActionConsequence)
import           Model.GID              (GID)

-- | Template function for looking at objects that support or contain other objects
-- Dynamically builds description based on current spatial relationships
-- Takes flavor text to add personality to the base object description
supportLookF :: GID Object -> Text -> DirectionalStimulusActionF
supportLookF objGID flavorText = DirectionalStimulusActionF (const (const supportLook'))
  where
    supportLook' :: GameComputation Identity ()
    supportLook' = do
      obj <- getObjectM objGID
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

      case Map.lookup objGID spatialMap of
        Nothing -> do
          -- Object has no spatial relationships, show base description + flavor text
          let msg = _description obj <> " " <> flavorText
          modifyNarration $ updateActionConsequence msg
        Just relationships -> do
          -- Get supported and contained objects
          supportedObjects <- getSupportedObjectNames objGID relationships
          containedObjects <- getContainedObjectNames objGID relationships

          -- Build dynamic description
          let baseDescription = _description obj
              fullDescription = buildFullDescription baseDescription flavorText supportedObjects containedObjects

          modifyNarration $ updateActionConsequence fullDescription

-- | Extract supported objects and get their short names
getSupportedObjectNames :: GID Object -> Set.Set SpatialRelationship -> GameComputation Identity [Text]
getSupportedObjectNames _objGID relationships = do
  let supportedGIDs = concatMap extractSupported (Set.toList relationships)
  mapM getObjectShortName supportedGIDs
  where
    extractSupported :: SpatialRelationship -> [GID Object]
    extractSupported (Supports oidSet) = Set.toList oidSet
    extractSupported _                 = []

-- | Extract contained objects and get their short names
getContainedObjectNames :: GID Object -> Set.Set SpatialRelationship -> GameComputation Identity [Text]
getContainedObjectNames _objGID relationships = do
  let containedGIDs = concatMap extractContained (Set.toList relationships)
  mapM getObjectShortName containedGIDs
  where
    extractContained :: SpatialRelationship -> [GID Object]
    extractContained (Contains oidSet) = Set.toList oidSet
    extractContained _                 = []

-- | Get the short name of an object
getObjectShortName :: GID Object -> GameComputation Identity Text
getObjectShortName oid = do
  obj <- getObjectM oid
  pure $ _shortName obj

-- | Build the full description with supported/contained objects and flavor text
buildFullDescription :: Text -> Text -> [Text] -> [Text] -> Text
buildFullDescription baseDescription flavorText supportedObjects containedObjects =
  let supportText = case supportedObjects of
        []     -> ""
        [item] -> " On the " <> baseDescription <> " is " <> item <> "."
        items  -> " On the " <> baseDescription <> " are " <> Text.intercalate ", " items <> "."

      containText = case containedObjects of
        []     -> ""
        [item] -> " In the " <> baseDescription <> " is " <> item <> "."
        items  -> " In the " <> baseDescription <> " are " <> Text.intercalate ", " items <> "."

  in baseDescription <> " " <> flavorText <> supportText <> containText
