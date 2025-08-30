 module Build.BedPuzzle.Actions.Look where
import           Control.Monad.State (gets)
import qualified Data.Map.Strict
import           GameState           (getObjectM, modifyNarration)
import           Model.GameState     (DirectionalStimulusActionF (ObjectDirectionalStimulusActionF),
                                      GameState (_world), Object,
                                      SpatialRelationship (Inventory),
                                      SpatialRelationshipMap,
                                      World (_spatialRelationshipMap),
                                      _shortName, updateActionConsequence)
import           Model.GID           (GID)


lookAtF :: GID Object -> DirectionalStimulusActionF
lookAtF objGID = ObjectDirectionalStimulusActionF lookAction
  where
    lookAction = do
      -- Get object info
      obj <- getObjectM objGID
      -- Get spatial relationships to determine location context
      world <- gets _world
      let spatialMap = _spatialRelationshipMap world
      case Data.Map.Strict.lookup objGID spatialMap of
        Just relationships -> generateLocationNarration obj relationships
        Nothing -> modifyNarration $ updateActionConsequence $ "You see the " <> _shortName obj

    generateLocationNarration obj relationships
      | Inventory `elem` relationships =
          modifyNarration $ updateActionConsequence $ "You're holding the " <> _shortName obj
      | Just containerGID <- findContainer relationships = do
          container <- getObjectM containerGID
          modifyNarration $ updateActionConsequence $ "The " <> _shortName obj <> " is in/on the " <> _shortName container
