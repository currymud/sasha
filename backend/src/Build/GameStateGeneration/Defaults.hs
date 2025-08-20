module Build.GameStateGeneration.Defaults  (defaultLocation
                                             ,defaultNarration
                                             ,defaultObject
                                             ,defaultPlayer
                                             ,defaultWorld) where
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                (Text)
import           Model.GameState          (ActionManagementFunctions (ActionManagementFunctions),
                                           Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                           Narration (Narration),
                                           Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                           Player (Player, _location, _playerActions),
                                           SpatialRelationshipMap (SpatialRelationshipMap),
                                           World (World, _locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap))
import           Model.GameState.Mappings (GIDToDataMap (GIDToDataMap))
import           Model.GID                (GID (GID))


defaultObject :: Object
defaultObject = Object
  { _shortName = mempty
  , _description = mempty
  , _descriptives = Data.Set.empty
  , _objectActionManagement = ActionManagementFunctions Data.Set.empty
  }

defaultLocation :: Location
defaultLocation = Location
  { _title = mempty
  , _objectSemanticMap = Data.Map.Strict.empty
  , _locationActionManagement = ActionManagementFunctions Data.Set.empty
  }

defaultWorld :: World
defaultWorld = World
  { _objectMap = GIDToDataMap Data.Map.Strict.empty
  , _locationMap = GIDToDataMap Data.Map.Strict.empty
  , _perceptionMap = mempty
  , _spatialRelationshipMap = SpatialRelationshipMap Data.Map.Strict.empty
  }

defaultPlayer :: Player
defaultPlayer = Player
  { _location = GID 0  -- Will be overridden
  , _playerActions = ActionManagementFunctions Data.Set.empty
  }

defaultNarration :: Narration
defaultNarration = Narration ["Let's begin" :: Text] mempty
