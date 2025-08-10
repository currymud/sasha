{-# OPTIONS_GHC -Wno-unused-imports #-}
module Build.World (world) where

import           Build.Identifiers.Locations (locationMap)
import           Build.Identifiers.Objects   (chairObjGID, objectMap,
                                              pillObjGID, pocketObjGID,
                                              robeObjGID, tableObjGID)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict
import           Data.Set                    (Set)
import qualified Data.Set
import           Model.GameState             (Location (Location),
                                              Object (Object),
                                              SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports),
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
     [ (chairObjGID, chairHolds)
     , (robeObjGID, Data.Set.fromList [robeHeld, robeContains])
     , (pocketObjGID, Data.Set.fromList [pocketContained, pocketContains])  -- Pocket contained in robe AND contains pill
     , (pillObjGID, Data.Set.singleton pillContained)  -- Pill is contained in pocket
     , (tableObjGID, Data.Set.singleton tableHolds) -- Table holds nothing
     ]
   chairHolds :: Set SpatialRelationship
   chairHolds = Data.Set.fromList [Supports $ Data.Set.fromList [robeObjGID]]

   robeContains :: SpatialRelationship
   robeContains = Contains $ Data.Set.fromList [pocketObjGID]


   tableHolds :: SpatialRelationship
   tableHolds = Supports Data.Set.empty
   -- Pocket is contained in robe and contains pill
   pocketContained :: SpatialRelationship
   pocketContained = ContainedIn robeObjGID
   pocketContains :: SpatialRelationship
   pocketContains = Contains $ Data.Set.fromList [pillObjGID]

   -- Pill is contained in pocket
   pillContained :: SpatialRelationship
   pillContained = ContainedIn pocketObjGID
   robeHeld :: SpatialRelationship
   robeHeld = SupportedBy chairObjGID
