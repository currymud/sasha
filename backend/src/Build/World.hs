{-# OPTIONS_GHC -Wno-unused-imports #-}
module Build.World (world) where

import           Build.Identifiers.Locations (locationMap)
import           Build.Identifiers.Objects   (objectMap)
import qualified Data.Map.Strict
import qualified Data.Set
import           Model.GameState             (Location, Object (Object),
                                              SpatialRelationshipMap (SpatialRelationshipMap),
                                              World (World))
import           Model.Mappings              (GIDToDataMap (GIDToDataMap))
world :: World
world = World objectMap locationMap mempty spatialRelationships
 where
   spatialRelationships :: SpatialRelationshipMap
   spatialRelationships =  SpatialRelationshipMap Data.Map.Strict.empty

chair :: Object
chair = Object "chair" "A wooden chair." Data.Set.empty Data.Map.Strict.empty
