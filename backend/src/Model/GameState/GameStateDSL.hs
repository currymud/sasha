{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.GameState.GameStateDSL where

import           Data.Kind                     (Type)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectMap, ActionKey,
                                                ActionManagement,
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
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                ConsumptionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Nouns (NounPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ConsumptionVerbPhrase)

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

  -- Builder constructors - now take direct values instead of WorldDSL wrappers
  BuildObject :: GID Object -> Object -> (Object -> Object) -> WorldDSL Object
  BuildLocation :: GID Location -> Location -> (Location -> Location) -> WorldDSL Location
  BuildPlayer :: Player -> (Player -> Player) -> WorldDSL Player

  -- Registration - now take direct values
  RegisterObject :: GID Object -> Object -> WorldDSL (GID Object)
  RegisterLocation :: GID Location -> Location -> WorldDSL (GID Location)

  -- ActionManagement construction - now take direct values
  CreateISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL ActionManagement
  CreateDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL ActionManagement
  CreateSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL ActionManagement
  CreateAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL ActionManagement
  CreateAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL ActionManagement
  CreateCAManagement :: ConsumptionVerbPhrase -> GID ConsumptionActionF -> WorldDSL ActionManagement
  CreatePPManagement :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement
  CreateNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement

  -- Player management - now take direct values
  SetPlayer :: Player -> WorldDSL ()
  UpdatePlayer :: Player -> (Player -> Player) -> WorldDSL Player

  -- Object/Player behavior attachment - now take direct values
  WithBehavior :: Object -> ActionManagement -> WorldDSL Object
  WithSpatial :: Object -> SpatialRelationship -> WorldDSL Object
  WithPlayerBehavior :: Player -> ActionManagement -> WorldDSL Player

  -- Location management
  ModifyLocation :: GID Location -> (Location -> Location) -> WorldDSL ()

  -- Spatial relationships - now take direct values
  SetSpatial :: GID Object -> SpatialRelationship -> WorldDSL ()

  -- Effect management
  CreateImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL Effect
  CreateDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL Effect
  CreateAcquisitionEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL Effect
  CreateConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> WorldDSL Effect
  CreatePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
  CreatePerceptionEffect :: WorldDSL Effect

  LinkEffectToObject :: GID Object -> Effect -> WorldDSL ()
  LinkEffectToLocation :: GID Location -> Effect -> WorldDSL ()
  LinkEffectToPlayer :: PlayerKey -> Effect -> WorldDSL ()
  RegisterEffectRegistry :: ActionKey -> ActionEffectMap -> WorldDSL ()
  ProcessEffectsIntoRegistry :: WorldDSL ()

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

-- Smart constructors - now clean without pure wrapping
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

-- Now take direct values instead of WorldDSL wrappers
buildObject :: GID Object -> Object -> (Object -> Object) -> WorldDSL Object
buildObject = BuildObject

buildLocation :: GID Location -> Location -> (Location -> Location) -> WorldDSL Location
buildLocation = BuildLocation

buildPlayer :: Player -> (Player -> Player) -> WorldDSL Player
buildPlayer = BuildPlayer

registerObject :: GID Object -> Object -> WorldDSL (GID Object)
registerObject = RegisterObject

registerLocation :: GID Location -> Location -> WorldDSL (GID Location)
registerLocation = RegisterLocation

setSpatial :: GID Object -> SpatialRelationship -> WorldDSL ()
setSpatial = SetSpatial

-- ActionManagement construction - now clean
createISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL ActionManagement
createISAManagement = CreateISAManagement

createDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL ActionManagement
createDSAManagement = CreateDSAManagement

createSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL ActionManagement
createSSAManagement = CreateSSAManagement

createAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL ActionManagement
createAAManagement = CreateAAManagement

createAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL ActionManagement
createAVManagement = CreateAVManagement

createCAManagement :: ConsumptionVerbPhrase -> GID ConsumptionActionF -> WorldDSL ActionManagement
createCAManagement = CreateCAManagement

createPPManagement :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement
createPPManagement = CreatePPManagement

createNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement
createNPManagement = CreateNPManagement

setPlayer :: Player -> WorldDSL ()
setPlayer = SetPlayer

updatePlayer :: Player -> (Player -> Player) -> WorldDSL Player
updatePlayer = UpdatePlayer

-- Behavior attachment - now clean
withBehavior :: Object -> ActionManagement -> WorldDSL Object
withBehavior = WithBehavior

withSpatial :: Object -> SpatialRelationship -> WorldDSL Object
withSpatial = WithSpatial

withPlayerBehavior :: Player -> ActionManagement -> WorldDSL Player
withPlayerBehavior = WithPlayerBehavior

modifyLocation :: GID Location -> (Location -> Location) -> WorldDSL ()
modifyLocation = ModifyLocation

processEffectsIntoRegistry :: WorldDSL ()
processEffectsIntoRegistry = ProcessEffectsIntoRegistry

-- Effect creation - now clean
createImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL Effect
createImplicitStimulusEffect = CreateImplicitStimulusEffect

createDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL Effect
createDirectionalStimulusEffect = CreateDirectionalStimulusEffect

createAcquisitionEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL Effect
createAcquisitionEffect = CreateAcquisitionEffect

createConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> WorldDSL Effect
createConsumptionEffect = CreateConsumptionEffect

createPosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
createPosturalEffect = CreatePosturalEffect

createPerceptionEffect :: WorldDSL Effect
createPerceptionEffect = CreatePerceptionEffect

linkEffectToObject :: GID Object -> Effect -> WorldDSL ()
linkEffectToObject = LinkEffectToObject

linkEffectToLocation :: GID Location -> Effect -> WorldDSL ()
linkEffectToLocation = LinkEffectToLocation

linkEffectToPlayer :: PlayerKey -> Effect -> WorldDSL ()
linkEffectToPlayer = LinkEffectToPlayer

registerEffectRegistry :: ActionKey -> ActionEffectMap -> WorldDSL ()
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

-- Batch operations built from primitives - now cleaner
registerObjects :: [(GID Object, Object)] -> WorldDSL [GID Object]
registerObjects = mapM (uncurry registerObject)

registerLocations :: [(GID Location, Location)] -> WorldDSL [GID Location]
registerLocations = mapM (uncurry registerLocation)

setSpatials :: [(GID Object, SpatialRelationship)] -> WorldDSL ()
setSpatials objectsAndRels = do
  sequence_ [setSpatial objGID rel | (objGID, rel) <- objectsAndRels]
