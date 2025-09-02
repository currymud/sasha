{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Build.GameStateGeneration.EDSL.GameStateBuilder where
import           Control.Monad                                                    (unless,
                                                                                   when)
import           Control.Monad.Except                                             (ExceptT,
                                                                                   MonadError,
                                                                                   runExceptT,
                                                                                   throwError)
import           Control.Monad.State.Strict                                       (MonadState,
                                                                                   State,
                                                                                   evalState,
                                                                                   execState,
                                                                                   get,
                                                                                   gets,
                                                                                   put)
import           Data.Kind                                                        (Type)
import qualified Data.List
import           Data.Map.Strict                                                  (Map,
                                                                                   elems,
                                                                                   empty,
                                                                                   findWithDefault,
                                                                                   fromList,
                                                                                   fromListWith,
                                                                                   insert,
                                                                                   insertWith,
                                                                                   lookup,
                                                                                   map,
                                                                                   member,
                                                                                   singleton,
                                                                                   unionWith)
import           Data.Set                                                         (Set)
import qualified Data.Set
import qualified Data.Text
import           Debug.Trace                                                      (trace)
import           GameState.Perception                                             (youSeeM)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (at)
import           Grammar.Parser.Partitions.Verbs.ImplicitRegionalStimulusVerb     (wait)
import           Model.GameState                                                  (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                                   ActionEffectMap (ActionEffectMap),
                                                                                   ActionGID (AcquisitionActionGID, ConsumptionActionGID, DirectionalActionGID, DirectionalContainerActionGID, ImplicitActionGID, PosturalActionGID, SomaticAccessActionGID),
                                                                                   ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, DSAContainerManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                                   ActionManagementOperation (AddAcquisitionVerb, AddAcquisitionVerbPhrase, AddConsumption, AddDirectionalContainerStimulus, AddDirectionalStimulus, AddImplicitStimulus, AddNegativePostural, AddPositivePostural, AddSomaticAccess),
                                                                                   ActionMaps (ActionMaps, _acquisitionActionMap, _consumptionActionMap, _directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap, _posturalActionMap, _somaticStimulusActionMap),
                                                                                   Effect (ActionManagementEffect, FieldUpdateEffect),
                                                                                   EffectActionKey (AcquisitionalActionKey, ConsumptionActionKey, DirectionalStimulusActionKey, DirectionalStimulusContainerActionKey, ImplicitStimulusActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                                                   EffectRegistry,
                                                                                   FieldUpdateOperation (LocationTitle, ObjectDescription, ObjectShortName, PlayerLocation),
                                                                                   GameState (_effectRegistry, _evaluation, _narration, _player, _systemEffectRegistry, _triggerRegistry),
                                                                                   Location (_objectSemanticMap, _title),
                                                                                   Narration (Narration),
                                                                                   Object (_description, _descriptives, _shortName),
                                                                                   Player (_location),
                                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                                   World (_locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap),
                                                                                   _actionSystemEffectKeys,
                                                                                   _locationActionManagement,
                                                                                   _objectActionManagement,
                                                                                   _playerActions,
                                                                                   _world)
import           Model.GameState.GameStateDSL                                     (WorldDSL (Apply, Bind, CreateAAManagement, CreateAVManagement, CreateAcquisitionVerbEffect, CreateAcquisitionVerbPhraseEffect, CreateCAManagement, CreateConsumptionEffect, CreateDSAContainerManagement, CreateDSAManagement, CreateDirectionalContainerStimulusEffect, CreateDirectionalStimulusEffect, CreateISAManagement, CreateImplicitStimulusEffect, CreateNPManagement, CreateNegativePosturalEffect, CreatePPManagement, CreatePositivePosturalEffect, CreateSSAManagement, CreateSomaticAccessEffect, DeclareAcquisitionActionGID, DeclareConsumableGID, DeclareConsumptionActionGID, DeclareContainerGID, DeclareDirectionalContainerActionGID, DeclareDirectionalStimulusActionGID, DeclareImplicitStimulusActionGID, DeclareLocationGID, DeclareObjectGID, DeclareObjectiveGID, DeclarePosturalActionGID, DeclareSomaticActionGID, DisplayVisibleObjects, FinalizeGameState, LinkActionKeyToSystemEffect, LinkEffectToLocation, LinkEffectToObject, LinkEffectToPlayer, LinkFieldEffectToLocation, LinkFieldEffectToObject, LinkFieldEffectToPlayer, Map, Pure, RegisterLocation, RegisterObject, RegisterObjectToLocation, RegisterPlayer, RegisterSpatial, RegisterSystemEffect, RegisterTrigger, Sequence, SetEvaluator, SetInitialNarration, SetPerceptionMap, UpdateDescription, UpdateLocation, UpdateShortName, UpdateTitle, WithDescription, WithDescriptives, WithLocationBehavior, WithObjectBehavior, WithPlayerBehavior, WithPlayerLocation, WithShortName, WithTitle))
import           Model.GameState.Mappings                                         (GIDToDataMap (GIDToDataMap, _getGIDToDataMap))
import           Model.GID                                                        (GID (GID))
import           Model.Parser.Atomics.Nouns                                       (Consumable,
                                                                                   Container,
                                                                                   DirectionalStimulus,
                                                                                   Objective)
import           Model.Parser.Composites.Nouns                                    (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
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
  , _nextImplicitActionGID :: Int
  , _nextDirectionalActionGID :: Int
  , _nextDirectionalContainerActionGID :: Int
  , _nextSomaticActionGID :: Int
  , _nextAcquisitionActionGID :: Int
  , _nextConsumptionActionGID :: Int
  , _nextPosturalActionGID :: Int
  , _actionMaps :: ActionMaps
  }

type WorldBuilderResult :: Type
data WorldBuilderResult = WorldBuilderResult
  { resultGameState  :: GameState
  , resultActionMaps :: ActionMaps
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
  , _nextImplicitActionGID = 1000
  , _nextDirectionalActionGID = 1000
  , _nextDirectionalContainerActionGID = 1000
  , _nextSomaticActionGID = 1000
  , _nextAcquisitionActionGID = 1000
  , _nextConsumptionActionGID = 1000
  , _nextPosturalActionGID = 1000
  , _actionMaps = ActionMaps
        { _implicitStimulusActionMap = Data.Map.Strict.empty
      , _directionalStimulusActionMap = Data.Map.Strict.empty
      , _directionalStimulusContainerActionMap = Data.Map.Strict.empty
      , _somaticStimulusActionMap = Data.Map.Strict.empty
      , _acquisitionActionMap = Data.Map.Strict.empty
      , _consumptionActionMap = Data.Map.Strict.empty
      , _posturalActionMap = Data.Map.Strict.empty
      }
  }

getActionMaps :: BuilderState -> ActionMaps
getActionMaps = _actionMaps
-- Initial builder state

runWorldBuilderWithMaps :: WorldBuilder a -> BuilderState -> Either WorldBuilderError WorldBuilderResult
runWorldBuilderWithMaps (WorldBuilder computation) initialState =
  case evalState (runExceptT computation) initialState of
    Left err -> Left err
    Right _ ->
      let finalState = execState (runExceptT computation) initialState
      in Right $ WorldBuilderResult
           { resultGameState = _gameState finalState
           , resultActionMaps = _actionMaps finalState
           }

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
      updatedState <- get  -- Get the state AFTER generateObjectGID
      put updatedState { _declaredObjectGIDs = Data.Map.Strict.insert nounPhrase newGID (_declaredObjectGIDs updatedState) }
      pure newGID

interpretDSL (DeclareObjectiveGID gid nounPhrase) = do
  state <- get
  -- Validate the provided GID exists in declared objects
  let allDeclaredObjectGIDs = concat
        [ Data.Map.Strict.elems (_declaredObjectGIDs state)
        , Data.Map.Strict.elems (_declaredObjectiveGIDs state)
        , Data.Map.Strict.elems (_declaredConsumableGIDs state)
        , Data.Map.Strict.elems (_declaredContainerGIDs state)
        ]
  unless (gid `elem` allDeclaredObjectGIDs) $
    throwError (InvalidObjectGID gid "Object GID not declared before grammatical registration")

  -- Check for duplicate noun phrase registration
  case Data.Map.Strict.lookup nounPhrase (_declaredObjectiveGIDs state) of
    Just existingGID -> throwError (DuplicateObjectGID existingGID "Objective noun phrase already registered")
    Nothing -> do
      put state { _declaredObjectiveGIDs = Data.Map.Strict.insert nounPhrase gid (_declaredObjectiveGIDs state) }
      pure ()

interpretDSL (DeclareConsumableGID gid nounPhrase) = do
  state <- get
  -- Validate the provided GID exists in declared objects
  let allDeclaredObjectGIDs = concat
        [ Data.Map.Strict.elems (_declaredObjectGIDs state)
        , Data.Map.Strict.elems (_declaredObjectiveGIDs state)
        , Data.Map.Strict.elems (_declaredConsumableGIDs state)
        , Data.Map.Strict.elems (_declaredContainerGIDs state)
        ]
  unless (gid `elem` allDeclaredObjectGIDs) $
    throwError (InvalidObjectGID gid "Object GID not declared before grammatical registration")

  -- Check for duplicate noun phrase registration
  case Data.Map.Strict.lookup nounPhrase (_declaredConsumableGIDs state) of
    Just existingGID -> throwError (DuplicateObjectGID existingGID "Consumable noun phrase already registered")
    Nothing -> do
      put state { _declaredConsumableGIDs = Data.Map.Strict.insert nounPhrase gid (_declaredConsumableGIDs state) }
      pure ()

interpretDSL (DeclareContainerGID gid nounPhrase) = do
  state <- get
  -- Validate the provided GID exists in declared objects
  let allDeclaredObjectGIDs = concat
        [ Data.Map.Strict.elems (_declaredObjectGIDs state)
        , Data.Map.Strict.elems (_declaredObjectiveGIDs state)
        , Data.Map.Strict.elems (_declaredConsumableGIDs state)
        , Data.Map.Strict.elems (_declaredContainerGIDs state)
        ]
  unless (gid `elem` allDeclaredObjectGIDs) $
    throwError (InvalidObjectGID gid "Object GID not declared before grammatical registration")

  -- Check for duplicate noun phrase registration
  case Data.Map.Strict.lookup nounPhrase (_declaredContainerGIDs state) of
    Just existingGID -> throwError (DuplicateObjectGID existingGID "Container noun phrase already registered")
    Nothing -> do
      put state { _declaredContainerGIDs = Data.Map.Strict.insert nounPhrase gid (_declaredContainerGIDs state) }
      pure ()

interpretDSL (DeclareLocationGID nounPhrase) = do
  state <- get
  case Data.Map.Strict.lookup nounPhrase (_declaredLocationGIDs state) of
    Just existingGID -> throwError (DuplicateLocationGID existingGID "Location GID already declared")
    Nothing -> do
      newGID <- generateLocationGID
      put state { _declaredLocationGIDs = Data.Map.Strict.insert nounPhrase newGID (_declaredLocationGIDs state) }
      pure newGID

interpretDSL (DeclareImplicitStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextImplicitActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_implicitStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _implicitStimulusActionMap = updatedMap }
  put state { _nextImplicitActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareDirectionalStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextDirectionalActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_directionalStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _directionalStimulusActionMap = updatedMap }
  put state { _nextDirectionalActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareDirectionalContainerActionGID actionF) = do
  state <- get
  let gidValue = _nextDirectionalContainerActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_directionalStimulusContainerActionMap currentMaps)
      updatedMaps = currentMaps { _directionalStimulusContainerActionMap = updatedMap }
  put state { _nextDirectionalContainerActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareSomaticActionGID actionF) = do
  state <- get
  let gidValue = _nextSomaticActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_somaticStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _somaticStimulusActionMap = updatedMap }
  put state { _nextSomaticActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareAcquisitionActionGID actionF) = do
  state <- get
  let gidValue = _nextAcquisitionActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_acquisitionActionMap currentMaps)
      updatedMaps = currentMaps { _acquisitionActionMap = updatedMap }
  put state { _nextAcquisitionActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareConsumptionActionGID actionF) = do
  state <- get
  let gidValue = _nextConsumptionActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_consumptionActionMap currentMaps)
      updatedMaps = currentMaps { _consumptionActionMap = updatedMap }
  put state { _nextConsumptionActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclarePosturalActionGID actionF) = do
  state <- get
  let gidValue = _nextPosturalActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_posturalActionMap currentMaps)
      updatedMaps = currentMaps { _posturalActionMap = updatedMap }
  put state { _nextPosturalActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (UpdateShortName text targetOid) =
  pure (FieldUpdateEffect (ObjectShortName targetOid text))

interpretDSL (UpdateDescription text targetOid) =
  pure (FieldUpdateEffect (ObjectDescription targetOid text))

interpretDSL (UpdateTitle text targetLid) =
  pure (FieldUpdateEffect (LocationTitle targetLid text))

interpretDSL (UpdateLocation targetLid) =
  pure (FieldUpdateEffect (PlayerLocation targetLid))
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

interpretDSL (RegisterSystemEffect sysEffectKey effectGID config) = do
  state <- get
  let currentGameState = _gameState state
      currentRegistry = _systemEffectRegistry currentGameState
      currentEffectMap = Data.Map.Strict.findWithDefault mempty sysEffectKey currentRegistry
      updatedEffectMap = Data.Map.Strict.insert effectGID config currentEffectMap
      updatedRegistry = Data.Map.Strict.insert sysEffectKey updatedEffectMap currentRegistry
      updatedGameState = currentGameState { _systemEffectRegistry = updatedRegistry }
  put state { _gameState = updatedGameState }

interpretDSL (RegisterTrigger actionKey sysEffectKey effectGID config) = do
 state <- get
 let currentGameState = _gameState state
     currentRegistry = _triggerRegistry currentGameState
     currentTriggers = Data.Map.Strict.findWithDefault [] actionKey currentRegistry
     newTrigger = (sysEffectKey, effectGID, config)
     updatedTriggers = newTrigger : currentTriggers
     updatedRegistry = Data.Map.Strict.insert actionKey updatedTriggers currentRegistry
     updatedGameState = currentGameState { _triggerRegistry = updatedRegistry }
 put state { _gameState = updatedGameState }

interpretDSL (LinkActionKeyToSystemEffect actionKey sysEffectKey) = do
  state <- get
  let currentGameState = _gameState state
      currentRegistry = _actionSystemEffectKeys currentGameState
      currentKeys = Data.Map.Strict.findWithDefault [] actionKey currentRegistry
      updatedKeys = sysEffectKey : currentKeys
      updatedRegistry = Data.Map.Strict.insert actionKey updatedKeys currentRegistry
      updatedGameState = currentGameState { _actionSystemEffectKeys = updatedRegistry }
  put state { _gameState = updatedGameState }

interpretDSL (LinkEffectToObject actionKey objGID effect) = do
  state <- get
  let effectKey = ObjectKey objGID
      currentEffectMap = Data.Map.Strict.findWithDefault (ActionEffectMap mempty) actionKey (_effectRegistry (_gameState state))
      ActionEffectMap currentMap = currentEffectMap
      updatedMap = Data.Map.Strict.insertWith Data.Set.union effectKey (Data.Set.singleton effect) currentMap
      updatedRegistry = Data.Map.Strict.insert actionKey (ActionEffectMap updatedMap) (_effectRegistry (_gameState state))
  put state { _gameState = (_gameState state) { _effectRegistry = updatedRegistry } }

interpretDSL (LinkEffectToLocation actionKey locGID effect) = do
  state <- get
  let effectKey = LocationKey locGID
      currentEffectMap = Data.Map.Strict.findWithDefault (ActionEffectMap mempty) actionKey (_effectRegistry (_gameState state))
      ActionEffectMap currentMap = currentEffectMap
      updatedMap = Data.Map.Strict.insertWith Data.Set.union effectKey (Data.Set.singleton effect) currentMap
      updatedRegistry = Data.Map.Strict.insert actionKey (ActionEffectMap updatedMap) (_effectRegistry (_gameState state))
  put state { _gameState = (_gameState state) { _effectRegistry = updatedRegistry } }

interpretDSL (LinkEffectToPlayer actionKey playerKey effect) = do
  state <- get
  let effectKey = PlayerKey playerKey
      currentEffectMap = Data.Map.Strict.findWithDefault (ActionEffectMap mempty) actionKey (_effectRegistry (_gameState state))
      ActionEffectMap currentMap = currentEffectMap
      updatedMap = Data.Map.Strict.insertWith Data.Set.union effectKey (Data.Set.singleton effect) currentMap
      updatedRegistry = Data.Map.Strict.insert actionKey (ActionEffectMap updatedMap) (_effectRegistry (_gameState state))
  put state { _gameState = (_gameState state) { _effectRegistry = updatedRegistry } }

interpretDSL (LinkFieldEffectToObject actionKey objGID effect) = do
 state <- get
 let effectKey = ObjectKey objGID
     currentEffectMap = Data.Map.Strict.findWithDefault (ActionEffectMap mempty) actionKey (_effectRegistry (_gameState state))
     ActionEffectMap currentMap = currentEffectMap
     updatedMap = Data.Map.Strict.insertWith Data.Set.union effectKey (Data.Set.singleton effect) currentMap
     updatedRegistry = Data.Map.Strict.insert actionKey (ActionEffectMap updatedMap) (_effectRegistry (_gameState state))
 put state { _gameState = (_gameState state) { _effectRegistry = updatedRegistry } }
interpretDSL FinalizeGameState = do
 gets _gameState

interpretDSL (LinkFieldEffectToLocation actionKey locGID fieldEffect) = do
 state <- get
 let effectKey = LocationKey locGID
     currentEffectMap = Data.Map.Strict.findWithDefault (ActionEffectMap mempty) actionKey (_effectRegistry (_gameState state))
     ActionEffectMap currentMap = currentEffectMap
     updatedMap = Data.Map.Strict.insertWith Data.Set.union effectKey (Data.Set.singleton fieldEffect) currentMap
     updatedRegistry = Data.Map.Strict.insert actionKey (ActionEffectMap updatedMap) (_effectRegistry (_gameState state))
 put state { _gameState = (_gameState state) { _effectRegistry = updatedRegistry } }

interpretDSL (LinkFieldEffectToPlayer actionKey playerKey fieldEffect) = do
 state <- get
 let effectKey = PlayerKey playerKey
     currentEffectMap = Data.Map.Strict.findWithDefault (ActionEffectMap mempty) actionKey (_effectRegistry (_gameState state))
     ActionEffectMap currentMap = currentEffectMap
     updatedMap = Data.Map.Strict.insertWith Data.Set.union effectKey (Data.Set.singleton fieldEffect) currentMap
     updatedRegistry = Data.Map.Strict.insert actionKey (ActionEffectMap updatedMap) (_effectRegistry (_gameState state))
 put state { _gameState = (_gameState state) { _effectRegistry = updatedRegistry } }

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

interpretDSL DisplayVisibleObjects = pure youSeeM

interpretDSL (CreateAcquisitionVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddAcquisitionVerb verb actionGID) (AcquisitionActionGID actionGID))

interpretDSL (CreateAcquisitionVerbPhraseEffect phrase actionGID) = do
  pure (ActionManagementEffect (AddAcquisitionVerbPhrase phrase actionGID) (AcquisitionActionGID actionGID))

interpretDSL (CreateConsumptionEffect verb objGID actionGID) = do
  pure (ActionManagementEffect (AddConsumption verb objGID actionGID) (ConsumptionActionGID actionGID))

interpretDSL (CreatePositivePosturalEffect verb actionGID) = do
  pure (ActionManagementEffect (AddPositivePostural verb actionGID) (PosturalActionGID actionGID))

interpretDSL (CreateNegativePosturalEffect verb actionGID) = do
  pure (ActionManagementEffect (AddNegativePostural verb actionGID) (PosturalActionGID actionGID))

interpretDSL (CreateSomaticAccessEffect verb actionGID) = do
  pure (ActionManagementEffect (AddSomaticAccess verb actionGID) (SomaticAccessActionGID actionGID))

interpretDSL (CreateImplicitStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddImplicitStimulus verb actionGID) (ImplicitActionGID actionGID))

interpretDSL (CreateDirectionalStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddDirectionalStimulus verb actionGID) (DirectionalActionGID actionGID))

interpretDSL (CreateDirectionalContainerStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddDirectionalContainerStimulus verb actionGID) (DirectionalContainerActionGID actionGID))

interpretDSL (SetPerceptionMap perceptionEntries) = do
  state <- get
  let perceptionMap = Data.Map.Strict.fromListWith Data.Set.union
        [(phrase, Data.Set.fromList gids) | (phrase, gids) <- perceptionEntries]
      updatedWorld = (_world (_gameState state)) { _perceptionMap = perceptionMap }
      updatedGameState = (_gameState state) { _world = updatedWorld }
  trace ("SetPerceptionMap: Creating perception map with entries: " ++ show perceptionEntries) $
    trace ("SetPerceptionMap: Resulting map: " ++ show perceptionMap) $
      put state { _gameState = updatedGameState }

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

interpretDSL (CreateDSAContainerManagement verb actionGID) =
  pure (DSAContainerManagementKey verb actionGID)

interpretDSL (CreateSSAManagement verb actionGID) =
  pure (SSAManagementKey verb actionGID)

interpretDSL (CreateAVManagement verb actionGID) =
  pure (AVManagementKey verb actionGID)

interpretDSL (CreateAAManagement verbPhrase actionGID) =
  pure (AAManagementKey verbPhrase actionGID)

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
  let updatedObj = obj { _descriptives = Data.Set.fromList $ fmap (DirectionalStimulusNounPhrase at) descriptives }
  pure updatedObj

-- Location field setter
interpretDSL (WithTitle text loc) = do
  let updatedLoc = loc { _title = text }
  pure updatedLoc

-- Player management

interpretDSL (WithPlayerLocation player locGID) =
  pure ( player { _location = locGID })

actionGIDToKey :: ActionGID -> EffectActionKey
actionGIDToKey (ImplicitActionGID gid)             = ImplicitStimulusActionKey gid
actionGIDToKey (DirectionalActionGID gid)          = DirectionalStimulusActionKey gid
actionGIDToKey (DirectionalContainerActionGID gid) = DirectionalStimulusContainerActionKey gid
actionGIDToKey (SomaticAccessActionGID gid)        = SomaticAccessActionKey gid
actionGIDToKey (AcquisitionActionGID gid)          = AcquisitionalActionKey gid
actionGIDToKey (ConsumptionActionGID gid)          = ConsumptionActionKey gid
actionGIDToKey (PosturalActionGID gid)             = PosturalActionKey gid
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
  trace ("generateObjectGID: current nextObjectGID: " ++ show (_nextObjectGID state) ++ ", generating: " ++ show newGID) $
    put state { _nextObjectGID = _nextObjectGID state + 1 }
  pure newGID

generateLocationGID :: WorldBuilder (GID Location)
generateLocationGID = do
  state <- get
  let newGID = GID (_nextLocationGID state)
  put state { _nextLocationGID = _nextLocationGID state + 1 }
  pure newGID
