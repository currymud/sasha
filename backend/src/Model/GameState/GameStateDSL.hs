module Model.GameState.GameStateDSL where

import           Data.Kind                     (Type)
import           Model.GameState               (AcquisitionActionF, ActionKey,
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                GameState,
                                                ImplicitStimulusActionF,
                                                Location, Object, Player,
                                                PosturalActionF,
                                                SomaticAccessActionF,
                                                SpatialRelationship)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (DirectionalStimulus)
import           Model.Parser.Composites.Nouns (NounPhrase)

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

-- GID Declaration constructors - let students create and bind their own names
  DeclareObjectGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Object)
  DeclareLocationGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Location)


  -- Core world building primitives - following current build system pattern
  BuildObject :: GID Object -> Object -> (Object -> Object) -> WorldDSL Object
  BuildLocation :: GID Location -> Location -> (Location -> Location) -> WorldDSL Location
  BuildPlayer :: Player -> (Player -> Player) -> WorldDSL Player

-- ActionKey construction - raw values are fine for constructors
  CreateImplicitStimulusActionKey :: GID ImplicitStimulusActionF -> WorldDSL ActionKey
  CreateDirectionalStimulusActionKey :: GID DirectionalStimulusActionF -> WorldDSL ActionKey
  CreateSomaticAccessActionKey :: GID SomaticAccessActionF -> WorldDSL ActionKey
  CreateAcquisitionActionKey :: GID AcquisitionActionF -> WorldDSL ActionKey
  CreateConsumptionActionKey :: GID ConsumptionActionF -> WorldDSL ActionKey
  CreatePosturalActionKey :: GID PosturalActionF -> WorldDSL ActionKey

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
