{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Build.GameStateGeneration.EDSL.GameStateBuilder where
import           Control.Monad                 (unless, when)
import           Control.Monad.Except          (ExceptT, MonadError, runExceptT,
                                                throwError)
import           Control.Monad.State           (State, evalState, get, put)
import           Control.Monad.State.Strict    (MonadState, runState)
import           Data.Kind                     (Type)
import qualified Data.List
import           Data.Map.Strict               (Map, elems, findWithDefault,
                                                fromListWith, insert, lookup,
                                                map, member, singleton, toList,
                                                unionWith)
import           Data.Set                      (Set)
import qualified Data.Set
import qualified Data.Text
import           Model.GameState               (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionKey (AcquisitionalActionKey, ConsumptionActionKey, DirectionalStimulusActionKey, ImplicitStimulusActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                Effect (AcquisitionPhraseEffect, AcquisitionVerbEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, NegativePosturalEffect, PerceptionEffect, PositivePosturalEffect, SomaticAccessEffect),
                                                EffectRegistry,
                                                GameState (_effectRegistry, _evaluation, _narration, _player),
                                                Location (_objectSemanticMap, _title),
                                                Narration (Narration),
                                                Object (_description, _descriptives, _shortName),
                                                Player (_location),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_locationMap, _objectMap, _spatialRelationshipMap),
                                                _locationActionManagement,
                                                _objectActionManagement,
                                                _playerActions, _world)
import           Model.GameState.GameStateDSL  (WorldDSL (Apply, Bind, CreateAAManagement, CreateAVManagement, CreateAcquisitionPhraseEffect, CreateAcquisitionVerbEffect, CreateCAManagement, CreateConsumptionEffect, CreateDSAManagement, CreateDirectionalStimulusEffect, CreateISAManagement, CreateImplicitStimulusEffect, CreateNPManagement, CreateNegativePosturalEffect, CreatePPManagement, CreatePerceptionEffect, CreatePositivePosturalEffect, CreateSSAManagement, DeclareConsumableGID, DeclareContainerGID, DeclareLocationGID, DeclareObjectGID, DeclareObjectiveGID, FinalizeGameState, LinkEffectToLocation, LinkEffectToObject, LinkEffectToPlayer, Map, Pure, RegisterLocation, RegisterObject, RegisterObjectToLocation, RegisterPlayer, RegisterSpatial, Sequence, SetEvaluator, SetInitialNarration, WithDescription, WithDescriptives, WithLocationBehavior, WithObjectBehavior, WithPlayerBehavior, WithPlayerLocation, WithShortName, WithTitle))
import           Model.GID                     (GID (GID))
import           Model.Mappings                (GIDToDataMap (GIDToDataMap, _getGIDToDataMap))
import           Model.Parser.Atomics.Nouns    (Consumable, Container,
                                                DirectionalStimulus, Objective)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase)

type BuilderState :: Type
data BuilderState = BuilderState
  { _gameState :: GameState
  , _nextObjectGID :: Int
  , _nextLocationGID :: Int
  , _declaredObjectGIDs :: Map (NounPhrase DirectionalStimulus) (GID Object)
  , _declaredObjectiveGIDs :: Map (NounPhrase Objective) (GID Object)
  , _declaredConsumableGIDs :: Map (NounPhrase Consumable) (GID Object)
  , _declaredContainerGIDs :: Map (NounPhrase Container) (GID Object)
  , _declaredLocationGIDs :: Map (NounPhrase DirectionalStimulus) (GID Location)
  , _createdEffects :: [Effect]                    -- All created effects
  , _effectLinks :: [(Effect, ActionEffectKey)]    -- Effect -> target mappings
  }

-- Update initial builder state
initialBuilderState :: GameState -> BuilderState
initialBuilderState gs = BuilderState
  { _gameState = gs
  , _nextObjectGID = 1
  , _nextLocationGID = 1
  , _declaredObjectGIDs = mempty
  , _declaredObjectiveGIDs = mempty
  , _declaredConsumableGIDs = mempty
  , _declaredContainerGIDs = mempty
  , _declaredLocationGIDs = mempty
  , _createdEffects = mempty
  , _effectLinks = mempty
  }
-- Initial builder state

type WorldBuilderError :: Type
data WorldBuilderError
 = NoPlayerBuilt String
 | PlayerAlreadyBuilt String
 | InvalidObjectGID (GID Object) String
 | InvalidLocationGID (GID Location) String
 | DuplicateObjectGID (GID Object) String
 | DuplicateLocationGID (GID Location) String
 | SpatialRelationshipError (GID Object) String
 | EmptyGameState String
 | InvalidPlayerLocation (GID Location) String
 deriving stock (Show, Eq, Ord)

-- | Error display
displayError :: WorldBuilderError -> String
displayError = \case
 NoPlayerBuilt ctx ->
   "No player built: " <> ctx
 PlayerAlreadyBuilt ctx ->
   "Player already exists: " <> ctx
 InvalidObjectGID gid ctx ->
   "Object GID " <> show gid <> " not found: " <> ctx
 InvalidLocationGID gid ctx ->
   "Location GID " <> show gid <> " not found: " <> ctx
 DuplicateObjectGID gid ctx ->
   "Duplicate object GID " <> show gid <> ": " <> ctx
 DuplicateLocationGID gid ctx ->
   "Duplicate location GID " <> show gid <> ": " <> ctx
 SpatialRelationshipError objGID ctx ->
   "Spatial relationship error for object " <> show objGID <> ": " <> ctx
 EmptyGameState ctx ->
   "Empty game state: " <> ctx
 InvalidPlayerLocation locGID ctx ->
   "Invalid player location " <> show locGID <> ": " <> ctx

type WorldBuilder :: Type -> Type
newtype WorldBuilder a = WorldBuilder
  (ExceptT WorldBuilderError (State BuilderState) a)
  deriving newtype ( Functor, Applicative, Monad
                   , MonadState BuilderState
                   , MonadError WorldBuilderError
                   )

-- Add this function to GameStateBuilder.hs
runWorldBuilder :: WorldBuilder a -> BuilderState -> Either WorldBuilderError a
runWorldBuilder (WorldBuilder computation) = evalState (runExceptT computation)

-- Core interpreter function
interpretDSL :: WorldDSL a -> WorldBuilder a
interpretDSL (Pure a)             = pure a

interpretDSL (Map f dsl)          = f <$> interpretDSL dsl

interpretDSL (Apply fDSL aDSL)    = interpretDSL fDSL <*> interpretDSL aDSL

interpretDSL (Sequence aDSL bDSL) = interpretDSL aDSL *> interpretDSL bDSL

interpretDSL (Bind aDSL f)        = interpretDSL aDSL >>= (interpretDSL . f)

interpretDSL (DeclareObjectGID nounPhrase) = do
  state <- get
  case Data.Map.Strict.lookup nounPhrase (_declaredObjectGIDs state) of
    Just existingGID -> throwError (DuplicateObjectGID existingGID "Object GID already declared")
    Nothing -> do
      newGID <- generateObjectGID
      put state { _declaredObjectGIDs = Data.Map.Strict.insert nounPhrase newGID (_declaredObjectGIDs state) }
      pure newGID

interpretDSL (DeclareObjectiveGID nounPhrase) = do
  state <- get
  case Data.Map.Strict.lookup nounPhrase (_declaredObjectiveGIDs state) of
    Just existingGID -> throwError (DuplicateObjectGID existingGID "Objective GID already declared")
    Nothing -> do
      newGID <- generateObjectGID
      put state { _declaredObjectiveGIDs = Data.Map.Strict.insert nounPhrase newGID (_declaredObjectiveGIDs state) }
      pure newGID

interpretDSL (DeclareConsumableGID nounPhrase) = do
  state <- get
  case Data.Map.Strict.lookup nounPhrase (_declaredConsumableGIDs state) of
    Just existingGID -> throwError (DuplicateObjectGID existingGID "Consumable GID already declared")
    Nothing -> do
      newGID <- generateObjectGID
      put state { _declaredConsumableGIDs = Data.Map.Strict.insert nounPhrase newGID (_declaredConsumableGIDs state) }
      pure newGID

interpretDSL (DeclareContainerGID nounPhrase) = do
  state <- get
  case Data.Map.Strict.lookup nounPhrase (_declaredContainerGIDs state) of
    Just existingGID -> throwError (DuplicateObjectGID existingGID "Container GID already declared")
    Nothing -> do
      newGID <- generateObjectGID
      put state { _declaredContainerGIDs = Data.Map.Strict.insert nounPhrase newGID (_declaredContainerGIDs state) }
      pure newGID

interpretDSL (DeclareLocationGID nounPhrase) = do
  state <- get
  case Data.Map.Strict.lookup nounPhrase (_declaredLocationGIDs state) of
    Just existingGID -> throwError (DuplicateLocationGID existingGID "Location GID already declared")
    Nothing -> do
      newGID <- generateLocationGID
      put state { _declaredLocationGIDs = Data.Map.Strict.insert nounPhrase newGID (_declaredLocationGIDs state) }
      pure newGID

interpretDSL (RegisterObject gid objDSL) = do
  obj <- interpretDSL objDSL
  state <- get
  let currentObjectMap = _getGIDToDataMap (_objectMap (_world (_gameState state)))
  let updatedObjectMap = Data.Map.Strict.insert gid obj currentObjectMap
      updatedWorld = (_world (_gameState state)) { _objectMap = GIDToDataMap updatedObjectMap }
      updatedGameState = (_gameState state) { _world = updatedWorld }
  put state { _gameState = updatedGameState }

interpretDSL (RegisterLocation gid locDSL) = do
  loc <- interpretDSL locDSL
  state <- get
  let currentLocationMap = _getGIDToDataMap (_locationMap (_world (_gameState state)))
  when (Data.Map.Strict.member gid currentLocationMap) $
    throwError (DuplicateLocationGID gid "Location already registered")
  let updatedLocationMap = Data.Map.Strict.insert gid loc currentLocationMap
      updatedWorld = (_world (_gameState state)) { _locationMap = GIDToDataMap updatedLocationMap }
      updatedGameState = (_gameState state) { _world = updatedWorld }
  put state { _gameState = updatedGameState }

interpretDSL (RegisterPlayer player) = do
  state <- get
  let updatedGameState = (_gameState state) { _player = player }
  put state { _gameState = updatedGameState }

interpretDSL (RegisterSpatial objGID spatialRel) = do
  state <- get
  let currentWorld = _world (_gameState state)
      SpatialRelationshipMap currentSpatialMap = _spatialRelationshipMap currentWorld
      currentRelationships = Data.Map.Strict.findWithDefault Data.Set.empty objGID currentSpatialMap
      updatedRelationships = Data.Set.insert spatialRel currentRelationships
      updatedSpatialMap = Data.Map.Strict.insert objGID updatedRelationships currentSpatialMap
      updatedWorld = currentWorld { _spatialRelationshipMap = SpatialRelationshipMap updatedSpatialMap }
      updatedGameState = (_gameState state) { _world = updatedWorld }
  put state { _gameState = updatedGameState }

interpretDSL (LinkEffectToObject objGID effect) = do
  state <- get
  let newLink = (effect, ObjectKey objGID)
  put state { _effectLinks = newLink : _effectLinks state }

interpretDSL (LinkEffectToLocation locGID effect) = do
  state <- get
  let newLink = (effect, LocationKey locGID)
  put state { _effectLinks = newLink : _effectLinks state }

interpretDSL (LinkEffectToPlayer playerKey effect) = do
  state <- get
  let newLink = (effect, PlayerKey playerKey)
  put state { _effectLinks = newLink : _effectLinks state }

interpretDSL FinalizeGameState = do
  state <- get
  let assembledEffectRegistry = buildEffectRegistryFromLinks (_effectLinks state)
      finalGameState = (_gameState state) { _effectRegistry = assembledEffectRegistry }
  pure finalGameState

interpretDSL (RegisterObjectToLocation locGID objGID nounKey) = do
  state <- get
  let currentLocationMap = _getGIDToDataMap (_locationMap (_world (_gameState state)))
  case Data.Map.Strict.lookup locGID currentLocationMap of
    Nothing -> throwError (InvalidLocationGID locGID "Location not registered")
    Just loc -> do
      let currentSemanticMap = _objectSemanticMap loc
          currentObjects = Data.Map.Strict.findWithDefault Data.Set.empty nounKey currentSemanticMap
          updatedObjects = Data.Set.insert objGID currentObjects
          updatedSemanticMap = Data.Map.Strict.insert nounKey updatedObjects currentSemanticMap
          updatedLoc = loc { _objectSemanticMap = updatedSemanticMap }
          updatedLocationMap = Data.Map.Strict.insert locGID updatedLoc currentLocationMap
          updatedWorld = (_world (_gameState state)) { _locationMap = GIDToDataMap updatedLocationMap }
          updatedGameState = (_gameState state) { _world = updatedWorld }
      put state { _gameState = updatedGameState }

interpretDSL (CreateImplicitStimulusEffect verb actionGID) = do
  let effect = ImplicitStimulusEffect verb actionGID
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

interpretDSL (CreateDirectionalStimulusEffect verb actionGID) = do
  let effect = DirectionalStimulusEffect verb actionGID
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

-- For AcquisitionVerb (simple verb like "get")
interpretDSL (CreateAcquisitionVerbEffect verb actionGID) = do
  let effect = AcquisitionVerbEffect verb actionGID
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

-- For AcquisitionVerbPhrase (complex phrase like "get the robe")
interpretDSL (CreateAcquisitionPhraseEffect verbPhrase actionGID) = do
  let effect = AcquisitionPhraseEffect verbPhrase actionGID
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

interpretDSL (CreateConsumptionEffect verb objGID actionGID) = do
  let effect = ConsumptionEffect verb objGID actionGID
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

interpretDSL (CreatePositivePosturalEffect verb actionGID) = do
  let effect = PositivePosturalEffect verb actionGID
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

-- For negative postural actions (like "sit down")
interpretDSL (CreateNegativePosturalEffect verb actionGID) = do
  let effect = NegativePosturalEffect verb actionGID
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

-- PerceptionEffect is a singleton - no parameters needed
interpretDSL CreatePerceptionEffect = do
  let effect = PerceptionEffect
  state <- get
  put state { _createdEffects = effect : _createdEffects state }
  pure effect

interpretDSL (SetEvaluator evaluator) = do
  state <- get
  let updatedGameState = (_gameState state) { _evaluation = evaluator }
  put state { _gameState = updatedGameState }

interpretDSL (SetInitialNarration text) = do
  state <- get
  let initialNarration = Narration (Data.List.singleton text) mempty
      updatedGameState = (_gameState state) { _narration = initialNarration }
  put state { _gameState = updatedGameState }

interpretDSL (CreateISAManagement verb actionGID) =
  pure (ISAManagementKey verb actionGID)

interpretDSL (CreateDSAManagement verb actionGID) =
  pure (DSAManagementKey verb actionGID)

interpretDSL (CreateSSAManagement verb actionGID) =
  pure (SSAManagementKey verb actionGID)

interpretDSL (CreateAAManagement verbPhrase actionGID) =
  pure (AAManagementKey verbPhrase actionGID)

interpretDSL (CreateAVManagement verb actionGID) =
  pure (AVManagementKey verb actionGID)

interpretDSL (CreateCAManagement verbPhrase actionGID) =
  pure (CAManagementKey verbPhrase actionGID)

interpretDSL (CreatePPManagement verb actionGID) =
  pure (PPManagementKey verb actionGID)

interpretDSL (CreateNPManagement verb actionGID) =
  pure (NPManagementKey verb actionGID)

interpretDSL (WithObjectBehavior obj actionMgmt) = do
  let ActionManagementFunctions currentSet = _objectActionManagement obj
      updatedSet = Data.Set.insert actionMgmt currentSet
      updatedObj = obj { _objectActionManagement = ActionManagementFunctions updatedSet }
  pure updatedObj

interpretDSL (WithPlayerBehavior player actionMgmt) = do
  let ActionManagementFunctions currentSet = _playerActions player
      updatedSet = Data.Set.insert actionMgmt currentSet
      updatedPlayer = player { _playerActions = ActionManagementFunctions updatedSet }
  pure updatedPlayer

interpretDSL (WithLocationBehavior loc actionMgmt) = do
  let ActionManagementFunctions currentSet = _locationActionManagement loc
      updatedSet = Data.Set.insert actionMgmt currentSet
      updatedLoc = loc { _locationActionManagement = ActionManagementFunctions updatedSet }
  pure updatedLoc

-- DSL Field Setter Implementations for Step 8
-- Add these to the interpretDSL function in GameStateBuilder.hs

-- Object field setters
interpretDSL (WithShortName text obj) = do
  let updatedObj = obj { _shortName = Data.Text.take 10 text }
  pure updatedObj

interpretDSL (WithDescription text obj) = do
  let updatedObj = obj { _description = text }
  pure updatedObj

interpretDSL (WithDescriptives descriptives obj) = do
  let updatedObj = obj { _descriptives = Data.Set.fromList $ fmap DirectionalStimulusNounPhrase descriptives }
  pure updatedObj

-- Location field setter
interpretDSL (WithTitle text loc) = do
  let updatedLoc = loc { _title = text }
  pure updatedLoc

-- Player management

interpretDSL (WithPlayerLocation locGID) = do
  state <- get
  -- Validate the location GID exists
  let currentPlayer = _player (_gameState state)
      updatedPlayer = currentPlayer { _location = locGID }
      updatedGameState = (_gameState state) { _player = updatedPlayer }
  put state { _gameState = updatedGameState }

-- Helper to validate object GID was declared
validateObjectGIDDeclared :: GID Object -> WorldBuilder ()
validateObjectGIDDeclared gid = do
  state <- get
  let allDeclaredObjectGIDs = concat
        [ Data.Map.Strict.elems (_declaredObjectGIDs state)
        , Data.Map.Strict.elems (_declaredObjectiveGIDs state)
        , Data.Map.Strict.elems (_declaredConsumableGIDs state)
        , Data.Map.Strict.elems (_declaredContainerGIDs state)
        ]
  unless (gid `elem` allDeclaredObjectGIDs) $
    throwError (InvalidObjectGID gid "Object GID not declared")

-- Helper to validate location GID was declared
validateLocationGIDDeclared :: GID Location -> WorldBuilder ()
validateLocationGIDDeclared gid = do
  state <- get
  let allDeclaredLocationGIDs = Data.Map.Strict.elems (_declaredLocationGIDs state)
  unless (gid `elem` allDeclaredLocationGIDs) $
    throwError (InvalidLocationGID gid "Location GID not declared")

-- Helper functions for GID generation
generateObjectGID :: WorldBuilder (GID Object)
generateObjectGID = do
  state <- get
  let newGID = GID (_nextObjectGID state)
  put state { _nextObjectGID = _nextObjectGID state + 1 }
  pure newGID

buildEffectRegistryFromLinks :: [(Effect, ActionEffectKey)] -> EffectRegistry
buildEffectRegistryFromLinks links =
  Data.Map.Strict.map ActionEffectMap $
  Data.Map.Strict.fromListWith (Data.Map.Strict.unionWith Data.Set.union) $
  fmap buildEntry links
  where
    buildEntry :: (Effect, ActionEffectKey) -> (ActionKey, Map ActionEffectKey (Set Effect))
    buildEntry (effect, effectKey) =
      (extractActionKey effect, Data.Map.Strict.singleton effectKey (Data.Set.singleton effect))

    extractActionKey :: Effect -> ActionKey
    extractActionKey = \case
      ImplicitStimulusEffect _ actionGID -> ImplicitStimulusActionKey actionGID
      DirectionalStimulusEffect _ actionGID -> DirectionalStimulusActionKey actionGID
      SomaticAccessEffect _ actionGID -> SomaticAccessActionKey actionGID
      AcquisitionVerbEffect _ actionGID -> AcquisitionalActionKey actionGID
      AcquisitionPhraseEffect _ actionGID -> AcquisitionalActionKey actionGID
      ConsumptionEffect _ _ actionGID -> ConsumptionActionKey actionGID
      PositivePosturalEffect _ actionGID -> PosturalActionKey actionGID
      NegativePosturalEffect _ actionGID -> PosturalActionKey actionGID
      PerceptionEffect -> error "PerceptionEffect needs special handling"

generateLocationGID :: WorldBuilder (GID Location)
generateLocationGID = do
  state <- get
  let newGID = GID (_nextLocationGID state)
  put state { _nextLocationGID = _nextLocationGID state + 1 }
  pure newGID
