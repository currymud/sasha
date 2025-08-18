{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Build.GameStateGeneration.EDSL.GameStateBuilder where
import           Control.Monad                 (unless, when)
import           Control.Monad.Except          (ExceptT, MonadError, throwError)
import           Control.Monad.State           (State, get, put)
import           Control.Monad.State.Strict    (MonadState)
import           Data.Kind                     (Type)
import           Data.Map.Strict               (Map, elems, insert, lookup,
                                                member)
import           Model.GameState               (GameState, Location, Object,
                                                World (_locationMap, _objectMap),
                                                _world)
import           Model.GameState.GameStateDSL  (WorldDSL (Apply, Bind, DeclareConsumableGID, DeclareContainerGID, DeclareLocationGID, DeclareObjectGID, DeclareObjectiveGID, Map, Pure, RegisterLocation, RegisterObject, Sequence))
import           Model.GID                     (GID (GID))
import           Model.Mappings                (GIDToDataMap (GIDToDataMap, _getGIDToDataMap))
import           Model.Parser.Atomics.Nouns    (Consumable, Container,
                                                DirectionalStimulus, Objective)
import           Model.Parser.Composites.Nouns (NounPhrase)

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

interpretDSL (RegisterObject gid obj) = do
  validateObjectGIDDeclared gid
  state <- get
  let currentObjectMap = _getGIDToDataMap (_objectMap (_world (_gameState state)))
  when (Data.Map.Strict.member gid currentObjectMap) $
    throwError (DuplicateObjectGID gid "Object already registered")

  let updatedObjectMap = Data.Map.Strict.insert gid obj currentObjectMap
      updatedWorld = (_world (_gameState state)) { _objectMap = GIDToDataMap updatedObjectMap }
      updatedGameState = (_gameState state) { _world = updatedWorld }
  put state { _gameState = updatedGameState }
  pure gid

interpretDSL (RegisterLocation gid loc) = do
  validateLocationGIDDeclared gid
  state <- get
  let currentLocationMap = _getGIDToDataMap (_locationMap (_world (_gameState state)))
  when (Data.Map.Strict.member gid currentLocationMap) $
    throwError (DuplicateLocationGID gid "Location already registered")

  let updatedLocationMap = Data.Map.Strict.insert gid loc currentLocationMap
      updatedWorld = (_world (_gameState state)) { _locationMap = GIDToDataMap updatedLocationMap }
      updatedGameState = (_gameState state) { _world = updatedWorld }
  put state { _gameState = updatedGameState }
  pure gid


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

generateLocationGID :: WorldBuilder (GID Location)
generateLocationGID = do
  state <- get
  let newGID = GID (_nextLocationGID state)
  put state { _nextLocationGID = _nextLocationGID state + 1 }
  pure newGID
