module Model.GameState.GameStateDSL where

import           Data.Kind       (Type)
import           Model.GameState (GameState, Location, Object, Player,
                                  SpatialRelationship)
import           Model.GID       (GID)

-- | World building DSL - designed to teach Functor -> Applicative -> Monad progression
type WorldDSL :: Type -> Type
data WorldDSL :: Type -> Type where
  -- Pure values (teaches Pure/return)
  Pure :: a -> WorldDSL a

  -- Functor operations (teaches <$>)
  Map :: (a -> b) -> WorldDSL a -> WorldDSL b

  -- Applicative operations (teaches <*> and sequential effects)
  Apply :: WorldDSL (a -> b) -> WorldDSL a -> WorldDSL b
  Sequence :: WorldDSL a -> WorldDSL b -> WorldDSL b  -- (*>)

  -- Monadic operations (teaches >>= and context-dependent computation)
  Bind :: WorldDSL a -> (a -> WorldDSL b) -> WorldDSL b

  -- Core world building primitives - following current build system pattern
  BuildObject :: GID Object -> Object -> (Object -> Object) -> WorldDSL Object
  BuildLocation :: GID Location -> Location -> (Location -> Location) -> WorldDSL Location
  BuildPlayer :: Player -> (Player -> Player) -> WorldDSL Player

  -- Register mappings - both single and collections
  RegisterObject :: GID Object -> Object -> WorldDSL ()
  RegisterObjects :: [(GID Object, Object)] -> WorldDSL ()
  RegisterLocation :: GID Location -> Location -> WorldDSL ()
  RegisterLocations :: [(GID Location, Location)] -> WorldDSL ()

  -- Spatial relationships - single only, collections built from primitives
  SetSpatial :: GID Object -> [SpatialRelationship] -> WorldDSL ()

  -- Context queries that demonstrate why we need monads
  GetCurrentObjects :: WorldDSL [Object]
  GetCurrentLocations :: WorldDSL [Location]
  GetCurrentPlayer :: WorldDSL Player

  -- Final assembly
  FinalizeGameState :: WorldDSL GameState

-- Instances that teach the hierarchy explicitly
instance Functor WorldDSL where
  fmap = Map

instance Applicative WorldDSL where
  pure = Pure
  (<*>) = Apply

instance Monad WorldDSL where
   return = pure
   (>>=) = Bind

buildSequentially :: WorldDSL a -> WorldDSL b -> WorldDSL b
buildSequentially = Sequence

setSpatial :: GID Object -> [SpatialRelationship] -> WorldDSL ()
setSpatial = SetSpatial

-- Collection operations built from GADT primitives - demonstrates composition
setSpatials :: [(GID Object, [SpatialRelationship])] -> WorldDSL ()
setSpatials = foldr (buildSequentially . uncurry setSpatial) (pure ())
