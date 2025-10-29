{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module EDSL.GameBuilder where
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
import           Data.List                                                        (singleton)
import           Data.Map.Strict                                                  (Map)
import qualified Data.Map.Strict                                                  (elems,
                                                                                   empty,
                                                                                   findWithDefault,
                                                                                   fromListWith,
                                                                                   insert,
                                                                                   insertWith,
                                                                                   lookup,
                                                                                   member)
import qualified Data.Set                                                         (empty,
                                                                                   fromList,
                                                                                   insert,
                                                                                   singleton,
                                                                                   union)
import           GameState.Perception                                             (youSeeM)
import           Grammar.Parser.Partitions.Prepositions.DirectionalStimulusMarker (atDS)
import           Model.Core                                                       (ActionEffectKey (AgentAcquisitionalActionKey, ConsumptionActionKey, ContainerAcquisitionalActionKey, LocationAcquisitionalActionKey, ObjectAcquisitionalActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                                                   ActionEffectMap (ActionEffectMap),
                                                                                   ActionGID (AgentAcquisitionActionGID, AgentContainerAccessActionGID, AgentDirectionalActionGID, AgentDirectionalContainerActionGID, AgentImplicitActionGID, ConsumptionActionGID, ContainerAcquisitionActionGID, ContainerDirectionalContainerActionGID, InstrumentContainerAccessActionGID, LocationAcquisitionActionGID, LocationContainerAccessActionGID, LocationDirectionalActionGID, LocationDirectionalContainerActionGID, LocationImplicitActionGID, ObjectAcquisitionActionGID, ObjectContainerAccessActionGID, ObjectDirectionalActionGID, PosturalActionGID, SomaticAccessActionGID),
                                                                                   ActionManagement (AgentConManagementKey, AgentDSAContainerManagementKey, AgentISAManagementKey, AgentSAConManagementKey, CAManagementKey, ContainerDSAContainerManagementKey, InstrumentConManagementKey, InstrumentSAConManagementKey, LocationConManagementKey, LocationDSAContainerManagementKey, LocationISAManagementKey, LocationSAConManagementKey, NPManagementKey, ObjectConManagementKey, ObjectSAConManagementKey, PPManagementKey, SSAManagementKey),
                                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                                   ActionManagementOperation (AddAgentAcquisitionVerb, AddAgentAcquisitionVerbPhrase, AddAgentContainerAccessSimpleVerb, AddAgentContainerAccessVerbPhrase, AddAgentDirectionalContainerStimulus, AddAgentDirectionalStimulus, AddAgentImplicitStimulus, AddConsumption, AddContainerAcquisitionVerb, AddContainerAcquisitionVerbPhrase, AddContainerDirectionalContainerStimulus, AddInstrumentContainerAccessSimpleVerb, AddInstrumentContainerAccessVerbPhrase, AddLocationAcquisitionVerb, AddLocationAcquisitionVerbPhrase, AddLocationContainerAccessSimpleVerb, AddLocationContainerAccessVerbPhrase, AddLocationDirectionalContainerStimulus, AddLocationDirectionalStimulus, AddLocationImplicitStimulus, AddNegativePostural, AddObjectAcquisitionVerb, AddObjectAcquisitionVerbPhrase, AddObjectContainerAccessSimpleVerb, AddObjectContainerAccessVerbPhrase, AddObjectDirectionalStimulus, AddPositivePostural, AddSomaticAccess),
                                                                                   ActionMaps (ActionMaps, _agentAcquisitionActionMap, _agentContainerAccessActionMap, _agentDirectionalStimulusActionMap, _agentDirectionalStimulusContainerActionMap, _agentImplicitStimulusActionMap, _consumptionActionMap, _containerAcquisitionActionMap, _containerDirectionalStimulusContainerActionMap, _instrumentContainerAccessActionMap, _locationAcquisitionActionMap, _locationContainerAccessActionMap, _locationDirectionalStimulusActionMap, _locationDirectionalStimulusContainerActionMap, _locationImplicitStimulusActionMap, _objectAcquisitionActionMap, _objectContainerAccessActionMap, _objectDirectionalStimulusActionMap, _posturalActionMap, _somaticStimulusActionMap),
                                                                                   Effect (ActionManagementEffect, FieldUpdateEffect),
                                                                                   FieldUpdateOperation (LocationTitle, ObjectDescription, ObjectShortName, PlayerLocation),
                                                                                   GameState (_actionSystemEffectKeys, _effectRegistry, _evaluation, _narration, _player, _systemEffectRegistry, _triggerRegistry, _world),
                                                                                   Location (_locationActionManagement, _locationInventory, _objectSemanticMap, _title),
                                                                                   Narration (Narration),
                                                                                   Object (_description, _descriptives, _objectActionManagement, _shortName),
                                                                                   Player (_location, _playerActions),
                                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                                   TargetEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                                   TriggerRegistry (TriggerRegistry, _unTriggerRegistry),
                                                                                   World (_globalSemanticMap, _locationMap, _objectMap, _perceptionMap, _spatialRelationshipMap))
import           Model.Core.Mappings                                              (GIDToDataMap (GIDToDataMap, _getGIDToDataMap))
import           Model.EDSL.SashaLambdaDSL                                        (SashaLambdaDSL (..))
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
  , _nextAgentImplicitStimulusActionGID :: Int
  , _nextLocationImplicitStimulusActionGID :: Int
  , _nextDirectionalActionGID :: Int
  , _nextAgentDirectionalStimulusActionGID :: Int
  , _nextObjectDirectionalStimulusActionGID :: Int
  , _nextLocationDirectionalStimulusActionGID :: Int
  , _nextDirectionalContainerActionGID :: Int
  , _nextAgentDirectionalContainerStimulusActionGID :: Int
  , _nextContainerDirectionalContainerStimulusActionGID :: Int
  , _nextLocationDirectionalContainerStimulusActionGID :: Int
  , _nextSomaticActionGID :: Int
  , _nextAcquisitionActionGID :: Int
  , _nextAgentAcquisitionActionGID :: Int
  , _nextObjectAcquisitionActionGID :: Int
  , _nextContainerAcquisitionActionGID :: Int
  , _nextLocationAcquisitionActionGID :: Int
  , _nextConsumptionActionGID :: Int
  , _nextContainerAccessActionGID :: Int
  , _nextAgentContainerAccessActionGID :: Int
  , _nextLocationContainerAccessActionGID :: Int
  , _nextObjectContainerAccessActionGID :: Int
  , _nextInstrumentContainerAccessActionGID :: Int
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
  , _nextAgentImplicitStimulusActionGID = 9000
  , _nextLocationImplicitStimulusActionGID = 10000
  , _nextDirectionalActionGID = 1000
  , _nextAgentDirectionalStimulusActionGID = 6000
  , _nextObjectDirectionalStimulusActionGID = 7000
  , _nextLocationDirectionalStimulusActionGID = 8000
  , _nextDirectionalContainerActionGID = 1000
  , _nextAgentDirectionalContainerStimulusActionGID = 11000
  , _nextContainerDirectionalContainerStimulusActionGID = 12000
  , _nextLocationDirectionalContainerStimulusActionGID = 13000
  , _nextSomaticActionGID = 1000
  , _nextAcquisitionActionGID = 1000
  , _nextAgentAcquisitionActionGID = 2000
  , _nextObjectAcquisitionActionGID = 3000
  , _nextContainerAcquisitionActionGID = 4000
  , _nextLocationAcquisitionActionGID = 5000
  , _nextConsumptionActionGID = 1000
  , _nextContainerAccessActionGID = 1000
  , _nextAgentContainerAccessActionGID = 1000
  , _nextLocationContainerAccessActionGID = 1000
  , _nextObjectContainerAccessActionGID = 1000
  , _nextInstrumentContainerAccessActionGID = 1000
  , _nextPosturalActionGID = 1000
  , _actionMaps = ActionMaps
        { _agentImplicitStimulusActionMap = Data.Map.Strict.empty
      , _locationImplicitStimulusActionMap = Data.Map.Strict.empty
      , _agentDirectionalStimulusActionMap = Data.Map.Strict.empty
      , _objectDirectionalStimulusActionMap = Data.Map.Strict.empty
      , _locationDirectionalStimulusActionMap = Data.Map.Strict.empty
      , _agentDirectionalStimulusContainerActionMap = Data.Map.Strict.empty
      , _containerDirectionalStimulusContainerActionMap = Data.Map.Strict.empty
      , _locationDirectionalStimulusContainerActionMap = Data.Map.Strict.empty
      , _agentContainerAccessActionMap = Data.Map.Strict.empty
      , _locationContainerAccessActionMap = Data.Map.Strict.empty
      , _objectContainerAccessActionMap = Data.Map.Strict.empty
      , _instrumentContainerAccessActionMap = Data.Map.Strict.empty
      , _somaticStimulusActionMap = Data.Map.Strict.empty
      , _agentAcquisitionActionMap = Data.Map.Strict.empty
      , _objectAcquisitionActionMap = Data.Map.Strict.empty
      , _containerAcquisitionActionMap = Data.Map.Strict.empty
      , _locationAcquisitionActionMap = Data.Map.Strict.empty
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
interpretDSL :: SashaLambdaDSL a -> WorldBuilder a
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

interpretDSL (DeclareAgentImplicitStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextAgentImplicitStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_agentImplicitStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _agentImplicitStimulusActionMap = updatedMap }
  put state { _nextAgentImplicitStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareLocationImplicitStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextLocationImplicitStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_locationImplicitStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _locationImplicitStimulusActionMap = updatedMap }
  put state { _nextLocationImplicitStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID


interpretDSL (DeclareAgentDirectionalStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextAgentDirectionalStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_agentDirectionalStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _agentDirectionalStimulusActionMap = updatedMap }
  put state { _nextAgentDirectionalStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareObjectDirectionalStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextObjectDirectionalStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_objectDirectionalStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _objectDirectionalStimulusActionMap = updatedMap }
  put state { _nextObjectDirectionalStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareLocationDirectionalStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextLocationDirectionalStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_locationDirectionalStimulusActionMap currentMaps)
      updatedMaps = currentMaps { _locationDirectionalStimulusActionMap = updatedMap }
  put state { _nextLocationDirectionalStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareAgentDirectionalContainerStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextAgentDirectionalContainerStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_agentDirectionalStimulusContainerActionMap currentMaps)
      updatedMaps = currentMaps { _agentDirectionalStimulusContainerActionMap = updatedMap }
  put state { _nextAgentDirectionalContainerStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareContainerDirectionalContainerStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextContainerDirectionalContainerStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_containerDirectionalStimulusContainerActionMap currentMaps)
      updatedMaps = currentMaps { _containerDirectionalStimulusContainerActionMap = updatedMap }
  put state { _nextContainerDirectionalContainerStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareLocationDirectionalContainerStimulusActionGID actionF) = do
  state <- get
  let gidValue = _nextLocationDirectionalContainerStimulusActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_locationDirectionalStimulusContainerActionMap currentMaps)
      updatedMaps = currentMaps { _locationDirectionalStimulusContainerActionMap = updatedMap }
  put state { _nextLocationDirectionalContainerStimulusActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareAgentContainerAccessActionGID actionF) = do
  state <- get
  let gidValue = _nextAgentContainerAccessActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_agentContainerAccessActionMap currentMaps)
      updatedMaps = currentMaps { _agentContainerAccessActionMap = updatedMap }
  put state { _nextAgentContainerAccessActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareLocationContainerAccessActionGID actionF) = do
  state <- get
  let gidValue = _nextLocationContainerAccessActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_locationContainerAccessActionMap currentMaps)
      updatedMaps = currentMaps { _locationContainerAccessActionMap = updatedMap }
  put state { _nextLocationContainerAccessActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareObjectContainerAccessActionGID actionF) = do
  state <- get
  let gidValue = _nextObjectContainerAccessActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_objectContainerAccessActionMap currentMaps)
      updatedMaps = currentMaps { _objectContainerAccessActionMap = updatedMap }
  put state { _nextObjectContainerAccessActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareInstrumentContainerAccessActionGID actionF) = do
  state <- get
  let gidValue = _nextInstrumentContainerAccessActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_instrumentContainerAccessActionMap currentMaps)
      updatedMaps = currentMaps { _instrumentContainerAccessActionMap = updatedMap }
  put state { _nextInstrumentContainerAccessActionGID = gidValue + 1
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


interpretDSL (DeclareAgentAcquisitionActionGID actionF) = do
  state <- get
  let gidValue = _nextAgentAcquisitionActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_agentAcquisitionActionMap currentMaps)
      updatedMaps = currentMaps { _agentAcquisitionActionMap = updatedMap }
  put state { _nextAgentAcquisitionActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareObjectAcquisitionActionGID actionF) = do
  state <- get
  let gidValue = _nextObjectAcquisitionActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_objectAcquisitionActionMap currentMaps)
      updatedMaps = currentMaps { _objectAcquisitionActionMap = updatedMap }
  put state { _nextObjectAcquisitionActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareContainerAcquisitionActionGID actionF) = do
  state <- get
  let gidValue = _nextContainerAcquisitionActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_containerAcquisitionActionMap currentMaps)
      updatedMaps = currentMaps { _containerAcquisitionActionMap = updatedMap }
  put state { _nextContainerAcquisitionActionGID = gidValue + 1
            , _actionMaps = updatedMaps }
  pure newGID

interpretDSL (DeclareLocationAcquisitionActionGID actionF) = do
  state <- get
  let gidValue = _nextLocationAcquisitionActionGID state
      newGID = GID gidValue
      currentMaps = _actionMaps state
      updatedMap = Data.Map.Strict.insert newGID actionF
                   (_locationAcquisitionActionMap currentMaps)
      updatedMaps = currentMaps { _locationAcquisitionActionMap = updatedMap }
  put state { _nextLocationAcquisitionActionGID = gidValue + 1
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
     currentRegistry = (_unTriggerRegistry . _triggerRegistry) currentGameState
     currentTriggers = Data.Map.Strict.findWithDefault [] actionKey currentRegistry
     newTrigger = (sysEffectKey, effectGID, config)
     updatedTriggers = newTrigger : currentTriggers
     updatedRegistry = TriggerRegistry $ Data.Map.Strict.insert actionKey updatedTriggers currentRegistry
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
      let
          currentSemanticMap = _objectSemanticMap loc
          currentObjects = Data.Map.Strict.findWithDefault Data.Set.empty nounKey currentSemanticMap
          updatedObjects = Data.Set.insert objGID currentObjects
          updatedSemanticMap = Data.Map.Strict.insert nounKey updatedObjects currentSemanticMap

          currentInventory = _locationInventory loc
          updatedInventory = Data.Set.insert objGID currentInventory

          updatedLoc = loc { _objectSemanticMap = updatedSemanticMap
                           , _locationInventory = updatedInventory }
          updatedLocationMap = Data.Map.Strict.insert locGID updatedLoc currentLocationMap
          -- Update global semantic map
          currentWorld = _world (_gameState state)
          currentGlobalMap = _globalSemanticMap currentWorld
          currentGlobalObjects = Data.Map.Strict.findWithDefault Data.Set.empty nounKey currentGlobalMap
          updatedGlobalObjects = Data.Set.insert objGID currentGlobalObjects
          updatedGlobalMap = Data.Map.Strict.insert nounKey updatedGlobalObjects currentGlobalMap
          -- Update world with both changes
          updatedWorld = currentWorld { _locationMap = GIDToDataMap updatedLocationMap
                                       , _globalSemanticMap = updatedGlobalMap }
          updatedGameState = (_gameState state) { _world = updatedWorld }
      put state { _gameState = updatedGameState }

interpretDSL DisplayVisibleObjects = pure youSeeM

interpretDSL (CreateConsumptionEffect verb objGID actionGID) = do
  pure (ActionManagementEffect (AddConsumption verb objGID actionGID) (ConsumptionActionGID actionGID))

interpretDSL (CreatePositivePosturalEffect verb actionGID) = do
  pure (ActionManagementEffect (AddPositivePostural verb actionGID) (PosturalActionGID actionGID))

interpretDSL (CreateNegativePosturalEffect verb actionGID) = do
  pure (ActionManagementEffect (AddNegativePostural verb actionGID) (PosturalActionGID actionGID))

interpretDSL (CreateSomaticAccessEffect verb actionGID) = do
  pure (ActionManagementEffect (AddSomaticAccess verb actionGID) (SomaticAccessActionGID actionGID))

interpretDSL (CreateAgentImplicitStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddAgentImplicitStimulus verb actionGID) (AgentImplicitActionGID actionGID))

interpretDSL (CreateLocationImplicitStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddLocationImplicitStimulus verb actionGID) (LocationImplicitActionGID actionGID))


interpretDSL (CreateAgentDirectionalStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddAgentDirectionalStimulus verb actionGID) (AgentDirectionalActionGID actionGID))

interpretDSL (CreateObjectDirectionalStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddObjectDirectionalStimulus verb actionGID) (ObjectDirectionalActionGID actionGID))

interpretDSL (CreateLocationDirectionalStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddLocationDirectionalStimulus verb actionGID) (LocationDirectionalActionGID actionGID))

interpretDSL (CreateAgentDirectionalContainerStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddAgentDirectionalContainerStimulus verb actionGID) (AgentDirectionalContainerActionGID actionGID))

interpretDSL (CreateContainerDirectionalContainerStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddContainerDirectionalContainerStimulus verb actionGID) (ContainerDirectionalContainerActionGID actionGID))

interpretDSL (CreateLocationDirectionalContainerStimulusEffect verb actionGID) = do
  pure (ActionManagementEffect (AddLocationDirectionalContainerStimulus verb actionGID) (LocationDirectionalContainerActionGID actionGID))

-- Role-based container access effect creation interpretations
interpretDSL (CreateAgentContainerAccessSimpleVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddAgentContainerAccessSimpleVerb verb actionGID) (AgentContainerAccessActionGID actionGID))

interpretDSL (CreateAgentContainerAccessVerbPhraseEffect cavp actionGID) = do
  pure (ActionManagementEffect (AddAgentContainerAccessVerbPhrase cavp actionGID) (AgentContainerAccessActionGID actionGID))

interpretDSL (CreateLocationContainerAccessSimpleVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddLocationContainerAccessSimpleVerb verb actionGID) (LocationContainerAccessActionGID actionGID))

interpretDSL (CreateLocationContainerAccessVerbPhraseEffect cavp actionGID) = do
  pure (ActionManagementEffect (AddLocationContainerAccessVerbPhrase cavp actionGID) (LocationContainerAccessActionGID actionGID))

interpretDSL (CreateObjectContainerAccessSimpleVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddObjectContainerAccessSimpleVerb verb actionGID) (ObjectContainerAccessActionGID actionGID))

interpretDSL (CreateObjectContainerAccessVerbPhraseEffect cavp actionGID) = do
  pure (ActionManagementEffect (AddObjectContainerAccessVerbPhrase cavp actionGID) (ObjectContainerAccessActionGID actionGID))

interpretDSL (CreateInstrumentContainerAccessSimpleVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddInstrumentContainerAccessSimpleVerb verb actionGID) (InstrumentContainerAccessActionGID actionGID))

interpretDSL (CreateInstrumentContainerAccessVerbPhraseEffect cavp actionGID) = do
  pure (ActionManagementEffect (AddInstrumentContainerAccessVerbPhrase cavp actionGID) (InstrumentContainerAccessActionGID actionGID))

-- Role-based acquisition effect creation interpretations
interpretDSL (CreateAgentAcquisitionVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddAgentAcquisitionVerb verb actionGID) (AgentAcquisitionActionGID actionGID))

interpretDSL (CreateObjectAcquisitionVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddObjectAcquisitionVerb verb actionGID) (ObjectAcquisitionActionGID actionGID))

interpretDSL (CreateContainerAcquisitionVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddContainerAcquisitionVerb verb actionGID) (ContainerAcquisitionActionGID actionGID))

interpretDSL (CreateLocationAcquisitionVerbEffect verb actionGID) = do
  pure (ActionManagementEffect (AddLocationAcquisitionVerb verb actionGID) (LocationAcquisitionActionGID actionGID))

interpretDSL (CreateAgentAcquisitionVerbPhraseEffect phrase actionGID) = do
  pure (ActionManagementEffect (AddAgentAcquisitionVerbPhrase phrase actionGID) (AgentAcquisitionActionGID actionGID))

interpretDSL (CreateObjectAcquisitionVerbPhraseEffect phrase actionGID) = do
  pure (ActionManagementEffect (AddObjectAcquisitionVerbPhrase phrase actionGID) (ObjectAcquisitionActionGID actionGID))

interpretDSL (CreateContainerAcquisitionVerbPhraseEffect phrase actionGID) = do
  pure (ActionManagementEffect (AddContainerAcquisitionVerbPhrase phrase actionGID) (ContainerAcquisitionActionGID actionGID))

interpretDSL (CreateLocationAcquisitionVerbPhraseEffect phrase actionGID) = do
  pure (ActionManagementEffect (AddLocationAcquisitionVerbPhrase phrase actionGID) (LocationAcquisitionActionGID actionGID))

interpretDSL (SetPerceptionMap perceptionEntries) = do
  state <- get
  let perceptionMap = Data.Map.Strict.fromListWith Data.Set.union
        [(phrase, Data.Set.fromList gids) | (phrase, gids) <- perceptionEntries]
      updatedWorld = (_world (_gameState state)) { _perceptionMap = perceptionMap }
      updatedGameState = (_gameState state) { _world = updatedWorld }
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

interpretDSL (CreateAgentISAManagement verb actionGID) =
  pure (AgentISAManagementKey verb actionGID)

interpretDSL (CreateLocationISAManagement verb actionGID) =
  pure (LocationISAManagementKey verb actionGID)

interpretDSL (CreateAgentDSAContainerManagement verb actionGID) =
  pure (AgentDSAContainerManagementKey verb actionGID)

interpretDSL (CreateContainerDSAContainerManagement verb actionGID) =
  pure (ContainerDSAContainerManagementKey verb actionGID)

interpretDSL (CreateLocationDSAContainerManagement verb actionGID) =
  pure (LocationDSAContainerManagementKey verb actionGID)

interpretDSL (CreateSSAManagement verb actionGID) =
  pure (SSAManagementKey verb actionGID)

interpretDSL (CreateCAManagement verbPhrase actionGID) =
  pure (CAManagementKey verbPhrase actionGID)

interpretDSL (CreatePPManagement verb actionGID) =
  pure (PPManagementKey verb actionGID)

interpretDSL (CreateNPManagement verb actionGID) =
  pure (NPManagementKey verb actionGID)

interpretDSL (CreateAgentSAConManagement verb actionGID) =
  pure (AgentSAConManagementKey verb actionGID)

interpretDSL (CreateLocationSAConManagement verb actionGID) =
  pure (LocationSAConManagementKey verb actionGID)

interpretDSL (CreateObjectSAConManagement verb actionGID) =
  pure (ObjectSAConManagementKey verb actionGID)

interpretDSL (CreateInstrumentSAConManagement verb actionGID) =
  pure (InstrumentSAConManagementKey verb actionGID)

interpretDSL (CreateAgentConManagement cavp actionGID) =
  pure (AgentConManagementKey cavp actionGID)

interpretDSL (CreateLocationConManagement cavp actionGID) =
  pure (LocationConManagementKey cavp actionGID)

interpretDSL (CreateObjectConManagement cavp actionGID) =
  pure (ObjectConManagementKey cavp actionGID)

interpretDSL (CreateInstrumentConManagement cavp actionGID) =
  pure (InstrumentConManagementKey cavp actionGID)

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

-- Object field setters
interpretDSL (WithShortName text obj) = do
  let updatedObj = obj { _shortName = text }
  pure updatedObj

interpretDSL (WithDescription text obj) = do
  let updatedObj = obj { _description = text }
  pure updatedObj

interpretDSL (WithDescriptives descriptives obj) = do
  let updatedObj = obj { _descriptives = Data.Set.fromList $ fmap (DirectionalStimulusNounPhrase atDS) descriptives }
  pure updatedObj

-- Location field setter
interpretDSL (WithTitle text loc) = do
  let updatedLoc = loc { _title = text }
  pure updatedLoc

-- Player management

interpretDSL (WithPlayerLocation player locGID) =
  pure ( player { _location = locGID })

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
