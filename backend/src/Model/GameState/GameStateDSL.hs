{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.GameState.GameStateDSL where

import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectMap, ActionKey,
                                                ActionManagement,
                                                ActionManagementFunctions,
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                Effect, Evaluator, GameState,
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
import           Model.Parser.GCase            (NounKey)

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

  -- Builder constructors - now take direct values instead of WorldDSL wrappers
  BuildObject :: GID Object -> Object -> (Object -> Object) -> WorldDSL Object
  BuildLocation :: GID Location -> Location -> (Location -> Location) -> WorldDSL Location
  BuildPlayer :: Player -> (Player -> Player) -> WorldDSL Player

  -- Registration - now take direct values
  RegisterObject :: GID Object -> Object -> WorldDSL (GID Object)
  RegisterLocation :: GID Location -> Location -> WorldDSL (GID Location)

  -- ActionManagement construction - returns individual management keys
  CreateISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL ActionManagement
  CreateDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL ActionManagement
  CreateSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL ActionManagement
  CreateAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL ActionManagement
  CreateAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL ActionManagement
  CreateCAManagement :: ConsumptionVerbPhrase -> GID ConsumptionActionF -> WorldDSL ActionManagement
  CreatePPManagement :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement
  CreateNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement

  SetEvaluator :: Evaluator -> WorldDSL ()
  SetInitialNarration :: Text -> WorldDSL ()
  -- Player management - now take direct values
  SetPlayer :: Player -> WorldDSL ()
  SetPlayerLocation :: GID Location -> WorldDSL ()  -- NEW: Clean player location setting
  UpdatePlayer :: Player -> (Player -> Player) -> WorldDSL Player

  WithBehavior :: Object -> ActionManagement -> WorldDSL Object
  WithSpatial :: Object -> SpatialRelationship -> WorldDSL Object
  WithPlayerBehavior :: Player -> ActionManagement -> WorldDSL Player

  -- Location management
  ModifyLocation :: GID Location -> (Location -> Location) -> WorldDSL ()

  -- NEW: Object-Location integration
  AddObjectToLocation :: GID Location -> GID Object -> NounKey -> WorldDSL ()

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

-- ActionManagement construction - FIXED: now returns ActionManagementFunctions
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

-- NEW: Clean player location setting
setPlayerLocation :: GID Location -> WorldDSL ()
setPlayerLocation = SetPlayerLocation

updatePlayer :: Player -> (Player -> Player) -> WorldDSL Player
updatePlayer = UpdatePlayer

withBehavior :: Object -> ActionManagement -> WorldDSL Object
withBehavior = WithBehavior

withSpatial :: Object -> SpatialRelationship -> WorldDSL Object
withSpatial = WithSpatial

withPlayerBehavior :: Player -> ActionManagement -> WorldDSL Player
withPlayerBehavior = WithPlayerBehavior

modifyLocation :: GID Location -> (Location -> Location) -> WorldDSL ()
modifyLocation = ModifyLocation

-- NEW: Object-Location integration
addObjectToLocation :: GID Location -> GID Object -> NounKey -> WorldDSL ()
addObjectToLocation = AddObjectToLocation

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

setEvaluator :: Evaluator -> WorldDSL ()
setEvaluator = SetEvaluator

setInitialNarration :: Text -> WorldDSL ()
setInitialNarration = SetInitialNarration

-- NEW: Batch object-location assignments
addObjectsToLocation :: GID Location -> [(GID Object, NounKey)] -> WorldDSL ()
addObjectsToLocation locationGID objectsWithKeys = do
  sequence_ [addObjectToLocation locationGID objGID nounKey | (objGID, nounKey) <- objectsWithKeys]
