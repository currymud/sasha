module Build.BedPuzzle.Actions.Look (lookAtF) where
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (gets)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text              (Text)
import           GameState              (getObjectM, modifyNarration)
import           GameState.Spatial      (getContainmentChain)
import           Model.GameState        (DirectionalStimulusActionF (DirectionalStimulusActionF),
                                         GameComputation, GameState (_world),
                                         Object (_description, _shortName),
                                         SpatialRelationship (ContainedIn),
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
