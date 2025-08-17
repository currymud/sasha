module Model.GameState.GameStateDSL where

import           Data.Kind       (Type)
import           Model.GameState (GameState, Location, Object, Player,
                                  SpatialRelationship)
import           Model.GID       (GID)

type WorldDSL :: Type -> Type
-- | World building DSL - designed to teach Functor -> Applicative -> Monad progression
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

 -- Core world building primitives
 BuildObject :: Object -> (Object -> Object) -> WorldDSL Object
 BuildLocation :: Location -> (Location -> Location) -> WorldDSL Location
 BuildPlayer :: Player -> (Player -> Player) -> WorldDSL Player
 SetSpatials :: [(GID Object, [SpatialRelationship])] -> WorldDSL ()

 -- Context queries that demonstrate why we need monads
 GetCurrentObjects :: WorldDSL [Object]
 GetCurrentLocations :: WorldDSL [Location]
 GetCurrentPlayer :: WorldDSL Player

 -- Final assembly
 FinalizeGameState :: WorldDSL GameState

-- Instances that teach the hierarchy explicitly
instance Functor WorldDSL where
 fmap = Map  -- Students see fmap implemented via GADT constructor

instance Applicative WorldDSL where
 pure = Pure  -- Students see pure implemented via GADT constructor
 (<*>) = Apply  -- Students see <*> implemented via GADT constructor

instance Monad WorldDSL where
 return = pure -- Alternative to pure
 (>>=) = Bind   -- Students see >>= implemented via GADT constructor

-- Domain-specific aliases for teaching
transformWorld :: (a -> b) -> WorldDSL a -> WorldDSL b
transformWorld = Map

combineActions :: WorldDSL (a -> b) -> WorldDSL a -> WorldDSL b
combineActions = Apply

buildSequentially :: WorldDSL a -> WorldDSL b -> WorldDSL b
buildSequentially = Sequence

threadResults :: WorldDSL a -> (a -> WorldDSL b) -> WorldDSL b
threadResults = Bind
