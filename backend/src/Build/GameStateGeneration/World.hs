module Build.GameStateGeneration.World where

import           Build.GameStateGeneration.LocationSpec.Locations (locationMap)
import           Build.GameStateGeneration.ObjectSpec             (defaultWorld)
import           Build.GameStateGeneration.ObjectSpec.Objects     (objectMap)

import qualified Data.Map.Strict
import           Model.GameState                                  (SpatialRelationshipMap (SpatialRelationshipMap),
                                                                   World (_locationMap, _objectMap, _spatialRelationshipMap))

-- MIGRATED: Using builder pattern instead of manual construction
world :: World
world = defaultWorld
  { _objectMap = objectMap      -- Use DSL-generated object map
  , _locationMap = locationMap  -- Use DSL-generated location map
  , _spatialRelationshipMap = spatialRelationships
  }
  where
    -- TODO: This should eventually be moved to the DSL system too
    -- For now, keeping minimal spatial relationships until full DSL migration
    spatialRelationships = SpatialRelationshipMap Data.Map.Strict.empty

-- The old manual construction is completely replaced with DSL-generated maps
-- No more hardcoded GID references, no more manual map construction
