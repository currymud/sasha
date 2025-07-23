{-# OPTIONS_GHC -Wno-typed-holes #-}
module Build.World (world) where

import           Build.Identifiers.Locations (locationMap)
import qualified Data.Map.Strict
import qualified Data.Set
import           Model.GameState             (Location, Object (Object),
                                              World (World))
import           Model.Mappings              (GIDToDataMap (GIDToDataMap))
world :: World
world = World objectMap locationMap'
 where
   locationMap' :: GIDToDataMap Location Location
   locationMap' = locationMap
   objectMap :: GIDToDataMap Object Object
   objectMap = GIDToDataMap mempty


chair :: Object
chair = Object "chair" "A wooden chair." Data.Set.empty Data.Map.Strict.empty
