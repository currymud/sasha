module Build.GameStateGeneration.WorldGeneration where

import           Data.Kind       (Type)
import           Model.GameState (ActionManagement, Location, Object,
                                  SpatialRelationship)
import           Model.GID       (GID)

-- Specification for object behaviors that TH can generate
type ObjectBehaviorSpec :: Type
data ObjectBehaviorSpec = ObjectBehaviorSpec
  { _objectGID :: GID Object
  , _behaviors :: [ActionManagement]
  }

type WorldSpec :: Type
data WorldSpec = WorldSpec
  { _objectSpecs          :: [ObjectBehaviorSpec]
  , _spatialRelationships :: [(GID Object, [SpatialRelationship])]
  , _locationSpecs        :: [(GID Location, Location)]
  , _playerSpec           :: PlayerSpec
  }

type PlayerSpec :: Type
data PlayerSpec = PlayerSpec
  { _playerLocationGID :: GID Location
  , _playerBehaviors   :: [ActionManagement]
  }

-- Future TH function signature:
-- makeWorld :: String -> WorldSpec -> Q [Dec]
-- makeWorld "myWorld" worldSpec = ...
