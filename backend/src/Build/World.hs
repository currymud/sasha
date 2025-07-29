{-# OPTIONS_GHC -Wno-unused-imports #-}
module Build.World (world) where

import           Build.Identifiers.Locations (locationMap)
import           Build.Identifiers.Objects   (objectMap)
import qualified Data.Map.Strict
import qualified Data.Set
import           Model.GameState             (Location (Location),
                                              Object (Object),
                                              SpatialRelationshipMap (SpatialRelationshipMap),
                                              World (World))
import           Model.Mappings              (GIDToDataMap (GIDToDataMap))
world :: World
world = World objectMap' locationMap' mempty spatialRelationships
 where
   objectMap' :: GIDToDataMap Object Object
   objectMap' = GIDToDataMap objectMap
   locationMap' :: GIDToDataMap Location Location
   locationMap' = GIDToDataMap locationMap
   spatialRelationships :: SpatialRelationshipMap
   spatialRelationships =  SpatialRelationshipMap Data.Map.Strict.empty

