{-# OPTIONS_GHC -Wno-typed-holes #-}
module Build.World (world) where

import           Build.Identifiers.Locations (locationMap)
import           Model.GameState             (Location, Object, World (World))
import           Model.Mappings              (GIDToDataMap (GIDToDataMap))
world :: World
world = World objectMap locationMap'
 where
   locationMap' :: GIDToDataMap Location Location
   locationMap' = locationMap
   objectMap :: GIDToDataMap Object Object
   objectMap = GIDToDataMap mempty
