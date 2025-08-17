module Model.GameState.GameStateDSL where

import           Data.Kind                     (Type)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectMap, ActionKey,
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                Effect, GameState,
                                                ImplicitStimulusActionF,
                                                Location, Object, Player,
                                                PlayerKey, PosturalActionF,
                                                SomaticAccessActionF,
                                                SpatialRelationship)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Consumable, Container,
                                                DirectionalStimulus, Objective)
import           Model.Parser.Atomics.Verbs    (ConsumptionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                PositivePosturalVerb)
import           Model.Parser.Composites.Nouns (NounPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase)

-- | World building DSL
type WorldDSL :: Type -> Type
data WorldDSL :: Type -> Type where
  -- Pure values
  Pure :: a -> WorldDSL a

  -- Functor operations
  Map :: (a -> b) -> WorldDSL a -> WorldDSL b

  -- Applicative operations
  Apply :: WorldDSL (a -> b) -> WorldDSL a -> WorldDSL b
  Sequence :: WorldDSL a -> WorldDSL b -> WorldDSL b

  -- Monadic operations
  Bind :: WorldDSL a -> (a -> WorldDSL b) -> WorldDSL b

  -- GID Declaration constructors
  DeclareObjectGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Object)
  DeclareObjectiveGID :: NounPhrase Objective -> WorldDSL (GID Object)
  DeclareConsumableGID :: NounPhrase Consumable -> WorldDSL (GID Object)
  DeclareContainerGID :: NounPhrase Container -> WorldDSL (GID Object)
  DeclareLocationGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Location)

  -- Initial value constructors
  InitialObject :: Object -> WorldDSL Object
  InitialLocation :: Location -> WorldDSL Location
  InitialPlayer :: Player -> WorldDSL Player

  -- Builder constructors - compositional with WorldDSL parameters
  BuildObject :: WorldDSL (GID Object) -> WorldDSL Object -> (Object -> Object) -> WorldDSL Object
  BuildLocation :: WorldDSL (GID Location) -> WorldDSL Location -> (Location -> Location) -> WorldDSL Location
  BuildPlayer :: WorldDSL Player -> (Player -> Player) -> WorldDSL Player

  -- Registration - returns GIDs for composition
  RegisterObject :: WorldDSL (GID Object) -> WorldDSL Object -> WorldDSL (GID Object)
  RegisterLocation :: WorldDSL (GID Location) -> WorldDSL Location -> WorldDSL (GID Location)

  -- ActionKey construction
  CreateImplicitStimulusActionKey :: GID ImplicitStimulusActionF -> WorldDSL ActionKey
  CreateDirectionalStimulusActionKey :: GID DirectionalStimulusActionF -> WorldDSL ActionKey
  CreateSomaticAccessActionKey :: GID SomaticAccessActionF -> WorldDSL ActionKey
  CreateAcquisitionActionKey :: GID AcquisitionActionF -> WorldDSL ActionKey
  CreateConsumptionActionKey :: GID ConsumptionActionF -> WorldDSL ActionKey
  CreatePosturalActionKey :: GID PosturalActionF -> WorldDSL ActionKey

  -- Effect creation
  CreateImplicitStimulusEffect :: WorldDSL ImplicitStimulusVerb -> WorldDSL (GID ImplicitStimulusActionF) -> WorldDSL Effect
  CreateDirectionalStimulusEffect :: WorldDSL DirectionalStimulusVerb -> WorldDSL (GID DirectionalStimulusActionF) -> WorldDSL Effect
  CreateAcquisitionEffect :: WorldDSL AcquisitionVerbPhrase -> WorldDSL (GID AcquisitionActionF) -> WorldDSL Effect
  CreateConsumptionEffect :: WorldDSL ConsumptionVerb -> WorldDSL (GID Object) -> WorldDSL (GID ConsumptionActionF) -> WorldDSL Effect
  CreatePosturalEffect :: WorldDSL PositivePosturalVerb -> WorldDSL (GID PosturalActionF) -> WorldDSL Effect
  CreatePerceptionEffect :: WorldDSL Effect

  -- Effect management
  LinkEffectToObject :: WorldDSL (GID Object) -> WorldDSL Effect -> WorldDSL ()
  LinkEffectToLocation :: WorldDSL (GID Location) -> WorldDSL Effect -> WorldDSL ()
  LinkEffectToPlayer :: WorldDSL PlayerKey -> WorldDSL Effect -> WorldDSL ()
  RegisterEffectRegistry :: WorldDSL ActionKey -> WorldDSL ActionEffectMap -> WorldDSL ()

  -- Spatial relationships - compositional
  SetSpatial :: WorldDSL (GID Object) -> WorldDSL [SpatialRelationship] -> WorldDSL ()

  -- Context queries
  GetCurrentObjects :: WorldDSL [Object]
  GetCurrentLocations :: WorldDSL [Location]
  GetCurrentPlayer :: WorldDSL Player

  -- Final assembly
  FinalizeGameState :: WorldDSL GameState

-- Instances
instance Functor WorldDSL where
  fmap = Map

instance Applicative WorldDSL where
  pure = Pure
  (<*>) = Apply

instance Monad WorldDSL where
   return = pure
   (>>=) = Bind

-- Smart constructors
declareObjectGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Object)
declareObjectGID = DeclareObjectGID

declareObjectiveGID :: NounPhrase Objective -> WorldDSL (GID Object)
declareObjectiveGID = DeclareObjectiveGID

declareConsumableGID :: NounPhrase Consumable -> WorldDSL (GID Object)
declareConsumableGID = DeclareConsumableGID

declareContainerGID :: NounPhrase Container -> WorldDSL (GID Object)
declareContainerGID = DeclareContainerGID

declareLocationGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Location)
declareLocationGID = DeclareLocationGID

initialObject :: Object -> WorldDSL Object
initialObject = InitialObject

initialLocation :: Location -> WorldDSL Location
initialLocation = InitialLocation

initialPlayer :: Player -> WorldDSL Player
initialPlayer = InitialPlayer

buildObject :: WorldDSL (GID Object) -> WorldDSL Object -> (Object -> Object) -> WorldDSL Object
buildObject = BuildObject

buildLocation :: WorldDSL (GID Location) -> WorldDSL Location -> (Location -> Location) -> WorldDSL Location
buildLocation = BuildLocation

buildPlayer :: WorldDSL Player -> (Player -> Player) -> WorldDSL Player
buildPlayer = BuildPlayer

registerObject :: WorldDSL (GID Object) -> WorldDSL Object -> WorldDSL (GID Object)
registerObject = RegisterObject

registerLocation :: WorldDSL (GID Location) -> WorldDSL Location -> WorldDSL (GID Location)
registerLocation = RegisterLocation

setSpatial :: WorldDSL (GID Object) -> WorldDSL [SpatialRelationship] -> WorldDSL ()
setSpatial = SetSpatial

createImplicitStimulusEffect :: WorldDSL ImplicitStimulusVerb -> WorldDSL (GID ImplicitStimulusActionF) -> WorldDSL Effect
createImplicitStimulusEffect = CreateImplicitStimulusEffect

createDirectionalStimulusEffect :: WorldDSL DirectionalStimulusVerb -> WorldDSL (GID DirectionalStimulusActionF) -> WorldDSL Effect
createDirectionalStimulusEffect = CreateDirectionalStimulusEffect

createAcquisitionEffect :: WorldDSL AcquisitionVerbPhrase -> WorldDSL (GID AcquisitionActionF) -> WorldDSL Effect
createAcquisitionEffect = CreateAcquisitionEffect

createConsumptionEffect :: WorldDSL ConsumptionVerb -> WorldDSL (GID Object) -> WorldDSL (GID ConsumptionActionF) -> WorldDSL Effect
createConsumptionEffect = CreateConsumptionEffect

createPosturalEffect :: WorldDSL PositivePosturalVerb -> WorldDSL (GID PosturalActionF) -> WorldDSL Effect
createPosturalEffect = CreatePosturalEffect

createPerceptionEffect :: WorldDSL Effect
createPerceptionEffect = CreatePerceptionEffect

linkEffectToObject :: WorldDSL (GID Object) -> WorldDSL Effect -> WorldDSL ()
linkEffectToObject = LinkEffectToObject

linkEffectToLocation :: WorldDSL (GID Location) -> WorldDSL Effect -> WorldDSL ()
linkEffectToLocation = LinkEffectToLocation

linkEffectToPlayer :: WorldDSL PlayerKey -> WorldDSL Effect -> WorldDSL ()
linkEffectToPlayer = LinkEffectToPlayer

registerEffectRegistry :: WorldDSL ActionKey -> WorldDSL ActionEffectMap -> WorldDSL ()
registerEffectRegistry = RegisterEffectRegistry

getCurrentObjects :: WorldDSL [Object]
getCurrentObjects = GetCurrentObjects

getCurrentLocations :: WorldDSL [Location]
getCurrentLocations = GetCurrentLocations

getCurrentPlayer :: WorldDSL Player
getCurrentPlayer = GetCurrentPlayer

finalizeGameState :: WorldDSL GameState
finalizeGameState = FinalizeGameState

-- Convenience functions
buildSequentially :: WorldDSL a -> WorldDSL b -> WorldDSL b
buildSequentially = Sequence

-- Batch operations built from primitives
registerObjects :: WorldDSL [(GID Object, Object)] -> WorldDSL [GID Object]
registerObjects objectsAction = do
  objects <- objectsAction
  mapM (\(gid, obj) -> registerObject (pure gid) (pure obj)) objects

registerLocations :: WorldDSL [(GID Location, Location)] -> WorldDSL [GID Location]
registerLocations locationsAction = do
  locations <- locationsAction
  mapM (\(gid, loc) -> registerLocation (pure gid) (pure loc)) locations

setSpatials :: WorldDSL [(GID Object, [SpatialRelationship])] -> WorldDSL ()
setSpatials objectsAndRels = do
  pairs <- objectsAndRels
  sequence_ [setSpatial (pure objGID) (pure rels) | (objGID, rels) <- pairs]
