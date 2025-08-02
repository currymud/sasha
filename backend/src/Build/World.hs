{-# OPTIONS_GHC -Wno-unused-imports #-}
module Build.World (world) where

import           Build.Identifiers.Locations (locationMap)
import           Build.Identifiers.Objects   (chairObjGID, objectMap)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict
import           Data.Set                    (Set)
import qualified Data.Set
import           Model.GameState             (Location (Location),
                                              Object (Object),
                                              SpatialRelationship (Supports),
                                              SpatialRelationshipMap (SpatialRelationshipMap),
                                              World (World))
import           Model.GID                   (GID)
import           Model.Mappings              (GIDToDataMap (GIDToDataMap))
world :: World
world = World objectMap' locationMap' mempty spatialRelationships
 where
   objectMap' :: GIDToDataMap Object Object
   objectMap' = GIDToDataMap objectMap
   locationMap' :: GIDToDataMap Location Location
   locationMap' = GIDToDataMap locationMap
   spatialRelationships :: SpatialRelationshipMap
   spatialRelationships =  SpatialRelationshipMap srMap
   srMap :: Map (GID Object) (Set SpatialRelationship)
   srMap = Data.Map.Strict.fromList
     [ (chairObjGID, Data.Set.fromList [Supports Data.Set.empty])]
