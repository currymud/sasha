{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Model.Core
  ( -- * Entity Types
    Player(..)
  , Location(..)
  , Object(..)
  , World(..)
  , Narration(..)
    -- * Spatial Types
  , SpatialRelationship(..)
  , SpatialRelationshipMap(..)
    -- * Core State and Config
  , GameState(..)
  , GameStateT(..)
  , GameComputation(..)
    -- * Transformers
  , GameT(..)
  , DisplayT(..)
  , transformToIO
  , liftDisplay
  , fromDisplay
  , liftToDisplay
  , identityToIO
    -- * Action Function Types
  , AgentImplicitStimulusActionF(..)
  , LocationImplicitStimulusActionF(..)
  , AgentDirectionalStimulusActionF(..)
  , ObjectDirectionalStimulusActionF(..)
  , LocationDirectionalStimulusActionF(..)
  , AgentDirectionalStimulusContainerActionF(..)
  , ContainerDirectionalStimulusContainerActionF(..)
  , LocationDirectionalStimulusContainerActionF(..)
  , ContainerAccessActionF(..)
  , AgentContainerAccessActionF(..)
  , LocationContainerAccessActionF(..)
  , ObjectContainerAccessActionF(..)
  , InstrumentContainerAccessActionF(..)
  , SomaticAccessActionF(..)
  , PosturalActionF(..)
  , AgentAcquisitionActionF(..)
  , ObjectAcquisitionActionF(..)
  , ContainerAcquisitionActionF(..)
  , LocationAcquisitionActionF(..)
  , ConsumptionActionF(..)
    -- * Action Maps
  , ActionMaps(..)
  , ActionEffectMap(..)
  , AgentImplicitStimulusActionMap
  , LocationImplicitStimulusActionMap
  , AgentDirectionalStimulusActionMap
  , LocationDirectionalStimulusActionMap
  , ObjectDirectionalStimulusActionMap
  , AgentDirectionalStimulusContainerActionMap
  , ContainerDirectionalStimulusContainerActionMap
  , LocationDirectionalStimulusContainerActionMap
  , ContainerAccessActionMap
  , AgentContainerAccessActionMap
  , LocationContainerAccessActionMap
  , ObjectContainerAccessActionMap
  , InstrumentContainerAccessActionMap
  , SomaticAccessActionMap
  , PosturalActionMap
  , AgentAcquisitionActionMap
  , ObjectAcquisitionActionMap
  , ContainerAcquisitionActionMap
  , LocationAcquisitionActionMap
  , ConsumptionActionMap
    -- * Action Management
  , ActionManagement(..)
  , ActionManagementFunctions(..)
  , ActionGID(..)
  , ActionManagementOperation(..)
    -- * Processing Types
  , SystemEffectMap
    -- * Search and Access Types
  , SimpleAccessSearchStrategy
  , SearchStrategy
  , FinalizeAccessNotInstrumentF
  , ContainerAccessF
  , AcquisitionF
    -- * Perception
  , Perceptables(..)
    -- * Field Updates
  , FieldUpdateOperation(..)
    -- * Effects and Registries
  , Evaluator(..)
  , PlayerKey(..)
  , ActionEffectKey(..)
  , TargetEffectKey(..)
  , SystemEffectKey(..)
  , NarrationComputation(..)
  , Effect(..)
  , SystemEffect(..)
  , SystemEffectConfig(..)
  , EffectRegistry
  , SystemEffectRegistry
  , SystemEffectKeysRegistry
  , TriggerRegistry(..)
  , ActionKeyMap(..)
    -- * Configuration
  , Config(..)
    -- * Intermediate Results
  , ActionEffectResult(..)
  , ConsumptionResult(..)
  , CoordinationResult(..)
  , AcquisitionRes(..)
  , CompleteAcquisitionRes(..)
  , SimpleAcquisitionRes(..)
  , AccessRes(..)
  , CompleteAccessRes(..)
  , SimpleAccessRes(..)
  , AgentDirectionalStimulusAction
  , ActionEffectKeyF
  ) where

import           Control.Monad.Except          (ExceptT, MonadError)
import           Control.Monad.Identity        (Identity, runIdentity)
import           Control.Monad.Morph           (MFunctor (hoist))
import           Control.Monad.Reader          (MonadReader, ReaderT)
import           Control.Monad.State           (MonadIO, MonadState, StateT)
import           Control.Monad.Trans           (MonadTrans (lift))
import           Data.Kind                     (Type)
import           Data.Map.Strict               (Map)
import           Data.Set                      (Set)
import           Data.Text                     (Text)
import           Model.Core.Mappings           (GIDToDataMap)
import           Model.GID                     (GID)
import           Model.Parser                  (Sentence)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                ConsumptionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SimpleAccessVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Nouns (ContainerPhrase,
                                                DirectionalStimulusNounPhrase,
                                                InstrumentalAccessNounPhrase,
                                                ObjectPhrase, SupportPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ConsumptionVerbPhrase,
                                                ContainerAccessVerbPhrase)
import           Model.Parser.GCase            (NounKey)
#ifdef TESTING
import           Data.Text                     (pack)
import           Model.GID                     (GID (GID))
import           Test.QuickCheck               (Arbitrary (arbitrary), Gen,
                                                choose, listOf, oneof, resize)
#endif

-- | State monad transformer for game state
type GameStateT :: (Type -> Type) -> Type -> Type
newtype GameStateT m a = GameStateT {runGameStateT :: StateT GameState m a}
  deriving newtype ( Functor
                   , Applicative
                   , MFunctor
                   , Monad
                   , MonadState GameState, MonadIO)

instance MonadTrans GameStateT where
  lift = GameStateT . lift

-- | Game monad with IO capabilities
type GameT :: (Type -> Type) -> Type -> Type
newtype GameT m a = GameT
  { runGameT :: ReaderT Config (ExceptT Text (GameStateT m)) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Config, MonadError Text, MonadState GameState, MonadIO
    )

instance MonadTrans GameT where
  lift = GameT . lift . lift . lift

-- | Display monad for rendering
type DisplayT :: (Type -> Type) -> Type -> Type
newtype DisplayT m a = DisplayT { runDisplayT :: GameStateT m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadState GameState)

instance MonadTrans DisplayT where
  lift = DisplayT . lift

-- | Convert Identity to IO
identityToIO :: Identity a -> IO a
identityToIO = return . runIdentity

-- | Transform GameComputation Identity to GameT IO
transformToIO :: GameComputation Identity a -> GameT IO a
transformToIO comp = GameT $ hoist (hoist (hoist identityToIO)) (runGameComputation comp)

-- | Lift DisplayT to GameT
liftDisplay :: (Monad m) => DisplayT m a -> GameT m a
liftDisplay display = GameT $ lift $ lift (runDisplayT display)

-- | Lift GameStateT to DisplayT
liftToDisplay :: GameStateT m a -> DisplayT m a
liftToDisplay = DisplayT

-- | Extract GameStateT from DisplayT
fromDisplay :: DisplayT m a -> GameStateT m a
fromDisplay = runDisplayT

-- | Game computation monad with reader, error, and state
type GameComputation :: (Type -> Type) -> Type -> Type
newtype GameComputation m a = GameComputation
  { runGameComputation :: ReaderT Config (ExceptT Text (GameStateT m)) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Config, MonadError Text, MonadState GameState
    )

instance MonadTrans GameComputation where
  lift = GameComputation . lift . lift . lift

-- Action function types
type AgentImplicitStimulusActionF :: Type
data AgentImplicitStimulusActionF
  = AgentCanSeeF (ActionEffectKey -> GameComputation Identity ())
  | AgentCannotSeeF (ActionEffectKey -> GameComputation Identity ())

type LocationImplicitStimulusActionF :: Type
data LocationImplicitStimulusActionF
  = LocationCanBeSeenImplicitF (ActionEffectKey -> GameComputation Identity ())
  | LocationCannotBeSeenImplicitF (ActionEffectKey -> GameComputation Identity ())

type AgentDirectionalStimulusAction :: Type
type AgentDirectionalStimulusAction
  = ActionEffectKey
      -> GameComputation Identity ()

type AgentDirectionalStimulusActionF :: Type
data AgentDirectionalStimulusActionF
  = AgentCanLookAtF AgentDirectionalStimulusAction
  | AgentCannotLookAtF (ActionEffectKey -> GameComputation Identity ())

type ObjectDirectionalStimulusActionF :: Type
data ObjectDirectionalStimulusActionF
  = ObjectCanBeSeenF (ActionEffectKey -> GameComputation Identity ())
  | ObjectCannotBeSeenF' (ActionEffectKey -> GameComputation Identity ())

type LocationDirectionalStimulusActionF :: Type
data LocationDirectionalStimulusActionF
  = LocationCanBeSeenF (ActionEffectKey -> GameComputation Identity ())
  | LocationCannotBeSeenF (ActionEffectKey -> GameComputation Identity ())

type ActionEffectKeyF :: Type
type ActionEffectKeyF = ActionEffectKey -> GameComputation Identity ()

type SimpleAccessSearchStrategy :: Type
type SimpleAccessSearchStrategy = NounKey
                                    -> GameComputation Identity (Maybe (GID Object))

type AgentDirectionalStimulusContainerActionF :: Type
data AgentDirectionalStimulusContainerActionF
  = AgentCanLookInF ActionEffectKeyF
  | AgentCannotLookInF ActionEffectKeyF

type LocationDirectionalStimulusContainerActionF :: Type
data LocationDirectionalStimulusContainerActionF
  = LocationCanBeSeenInF ActionEffectKeyF
  | LocationCannotBeSeenInF ActionEffectKeyF

type ContainerDirectionalStimulusContainerActionF :: Type
data ContainerDirectionalStimulusContainerActionF
  = ContainerCanBeSeenInF ActionEffectKeyF
  | ContainerCannotBeSeenInF' ActionEffectKeyF

-- | Unified result type for actions that produce effect keys
type ActionEffectResult :: Type
newtype ActionEffectResult = ActionEffectResult
  { _actionEffectKeys :: [ActionEffectKey]
  }
  deriving stock (Show, Eq, Ord)

type FinalizeAccessNotInstrumentF :: Type
type FinalizeAccessNotInstrumentF = ActionEffectKey
                                      -> GameComputation Identity ActionEffectKey
                                      -> GameComputation Identity ()

type ContainerAccessF :: Type
type ContainerAccessF
  = ActionEffectKey
      -> AccessRes
      -> ContainerAccessActionMap
      -> (ActionManagementFunctions -> Maybe (GID ContainerAccessActionF))
      -> GameComputation Identity ()

type ContainerAccessActionF :: Type
data ContainerAccessActionF
  = PlayerContainerAccessF ContainerAccessF
  | PlayerCannotAccessF (ActionEffectKey -> GameComputation Identity ())
  | ObjectContainerAccessF (ActionEffectKey -> GameComputation Identity ())
  | InstrumentContainerAccessF (ActionEffectKey -> GameComputation Identity ())
  | CannotAccessF (ActionEffectKey -> GameComputation Identity ())

type AgentContainerAccessActionF :: Type
data AgentContainerAccessActionF
  = AgentCanAccessF ActionEffectKeyF
  | AgentCannotAccessF ActionEffectKeyF

type LocationContainerAccessActionF :: Type
data LocationContainerAccessActionF
  = LocationCanAccessContainerF ActionEffectKeyF
  | LocationCannotAccessContainerF ActionEffectKeyF

type ObjectContainerAccessActionF :: Type
data ObjectContainerAccessActionF
  = ContainingObjectCanAccessF ActionEffectKeyF
  | ContainingObjectCannotAccessF ActionEffectKeyF

type InstrumentContainerAccessActionF :: Type
data InstrumentContainerAccessActionF
  = InstrumentCanAccessF ActionEffectKeyF
  | InstrumentCannotAccessF ActionEffectKeyF

type SomaticAccessActionF :: Type
data SomaticAccessActionF
  = PlayerSomaticAccessActionF (ActionEffectKey -> GameComputation Identity ())
  | CannotSomaticAccessF (ActionEffectKey -> GameComputation Identity ())

type PosturalActionF :: Type
data PosturalActionF
  = PlayerPosturalActionF (ActionEffectKey -> GameComputation Identity ())
  | CannotPosturalActionF (ActionEffectKey -> GameComputation Identity ())

type CoordinationResult :: Type
data CoordinationResult = CoordinationResult
  { _computation      :: GameComputation Identity ()
  , _actionEffectKeys :: [ActionEffectKey]
  }

-- | Acquisition parsing result - Simple vs Complete acquisition commands
type AcquisitionRes :: Type
data AcquisitionRes
  = Complete CompleteAcquisitionRes
  | Simple SimpleAcquisitionRes
  deriving stock (Show, Eq, Ord)

-- | Complete acquisition: "get X from Y"
type CompleteAcquisitionRes :: Type
data CompleteAcquisitionRes = CompleteAcquisitionRes
  { _caObjectKey     :: NounKey
  , _caObjectPhrase  :: ObjectPhrase
  , _caSupportKey    :: NounKey
  , _caSupportPhrase :: SupportPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Simple acquisition: "get X"
type SimpleAcquisitionRes :: Type
data SimpleAcquisitionRes = SimpleAcquisitionRes
  { _saObjectKey    :: NounKey
  , _saObjectPhrase :: ObjectPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Access parsing result - Simple vs Complete access commands
type AccessRes :: Type
data AccessRes
  = CompleteAR CompleteAccessRes
  | SimpleAR SimpleAccessRes
  deriving stock (Show, Eq, Ord)

-- | Complete access: "open X with Y"
type CompleteAccessRes :: Type
data CompleteAccessRes = CompleteAccessRes
  { _containerKey     :: NounKey
  , _ContainerPhrase  :: ContainerPhrase
  , _instrumentKey    :: NounKey
  , _instrumentPhrase :: InstrumentalAccessNounPhrase
  }
  deriving stock (Show, Eq, Ord)

-- | Simple access: "open X"
type SimpleAccessRes :: Type
data SimpleAccessRes = SimpleAccessRes
  { _saContainerKey    :: NounKey
  , _saContainerPhrase :: ContainerPhrase
  }
  deriving stock (Show, Eq, Ord)
type SearchStrategy :: Type
type SearchStrategy = NounKey
                        -> GameComputation Identity (Maybe (GID Object, GID Object))

type AcquisitionF :: Type
type AcquisitionF = (ActionEffectKey
                      -> AcquisitionRes
                      -> GameComputation Identity ())

-- Role-based acquisition action types
type AgentAcquisitionActionF :: Type
data AgentAcquisitionActionF
  = AgentAcquiresF AcquisitionF  -- Agent coordinates acquisition between object and container
  | AgentCannotAcquireF (ActionEffectKey -> GameComputation Identity ())

type ObjectAcquisitionActionF :: Type
data ObjectAcquisitionActionF
  = ObjectCollectedF (GameComputation Identity CoordinationResult)  -- Object is collected by agent
  | ObjectNotCollectableF (ActionEffectKey -> GameComputation Identity ())

type ContainerAcquisitionActionF :: Type
data ContainerAcquisitionActionF
  = ContainerLosesObjectF (GID Object -> GameComputation Identity CoordinationResult)  -- Container releases object
  | ContainerCannotReleaseF (ActionEffectKey -> GameComputation Identity ())

type LocationAcquisitionActionF :: Type
data LocationAcquisitionActionF
  = LocationAcquisitionActionF (GameComputation Identity ())  -- Placeholder for location-level acquisition
  | LocationCannotAcquireF (ActionEffectKey -> GameComputation Identity ())

type ConsumptionResult :: Type
data ConsumptionResult = ConsumedResult
  { _consumptionComputation :: GameComputation Identity ()
  , _consumptionEffectKeys  :: ActionEffectKey
  }

type ConsumptionActionF :: Type
data ConsumptionActionF
  = PlayerConsumptionActionF (ActionEffectKey -> GID Object -> ConsumptionVerbPhrase -> GameComputation Identity ())
  | PlayerCannotConsumeF (ActionEffectKey -> GameComputation Identity ())
  | ObjectConsumedF (ActionEffectKey -> GameComputation Identity ConsumptionResult)
  | ObjectCannotBeConsumedF (ActionEffectKey -> GameComputation Identity ActionEffectKey)

-- Action Maps

type AgentImplicitStimulusActionMap :: Type
type AgentImplicitStimulusActionMap = Map (GID AgentImplicitStimulusActionF) AgentImplicitStimulusActionF

type LocationImplicitStimulusActionMap :: Type
type LocationImplicitStimulusActionMap = Map (GID LocationImplicitStimulusActionF) LocationImplicitStimulusActionF

type AgentDirectionalStimulusContainerActionMap :: Type
type AgentDirectionalStimulusContainerActionMap = Map (GID AgentDirectionalStimulusContainerActionF) AgentDirectionalStimulusContainerActionF

type ContainerDirectionalStimulusContainerActionMap :: Type
type ContainerDirectionalStimulusContainerActionMap = Map (GID ContainerDirectionalStimulusContainerActionF) ContainerDirectionalStimulusContainerActionF

type LocationDirectionalStimulusContainerActionMap :: Type
type LocationDirectionalStimulusContainerActionMap = Map (GID LocationDirectionalStimulusContainerActionF) LocationDirectionalStimulusContainerActionF

type AgentContainerAccessActionMap :: Type
type AgentContainerAccessActionMap = Map (GID AgentContainerAccessActionF) AgentContainerAccessActionF

type LocationContainerAccessActionMap :: Type
type LocationContainerAccessActionMap = Map (GID LocationContainerAccessActionF) LocationContainerAccessActionF

type ObjectContainerAccessActionMap :: Type
type ObjectContainerAccessActionMap  = Map (GID ObjectContainerAccessActionF) ObjectContainerAccessActionF

type InstrumentContainerAccessActionMap :: Type
type InstrumentContainerAccessActionMap = Map (GID InstrumentContainerAccessActionF) InstrumentContainerAccessActionF

type ContainerAccessActionMap :: Type
type ContainerAccessActionMap = Map (GID ContainerAccessActionF) ContainerAccessActionF

type SomaticAccessActionMap :: Type
type SomaticAccessActionMap = Map (GID SomaticAccessActionF) SomaticAccessActionF

type SomaticStimulusActionMap :: Type
type SomaticStimulusActionMap = Map (GID SomaticAccessActionF) SomaticAccessActionF

-- Role-based acquisition action maps
type AgentAcquisitionActionMap :: Type
type AgentAcquisitionActionMap = Map (GID AgentAcquisitionActionF) AgentAcquisitionActionF

type ObjectAcquisitionActionMap :: Type
type ObjectAcquisitionActionMap = Map (GID ObjectAcquisitionActionF) ObjectAcquisitionActionF

type ContainerAcquisitionActionMap :: Type
type ContainerAcquisitionActionMap = Map (GID ContainerAcquisitionActionF) ContainerAcquisitionActionF

type LocationAcquisitionActionMap :: Type
type LocationAcquisitionActionMap = Map (GID LocationAcquisitionActionF) LocationAcquisitionActionF

type AgentDirectionalStimulusActionMap :: Type
type AgentDirectionalStimulusActionMap = Map (GID AgentDirectionalStimulusActionF) AgentDirectionalStimulusActionF

type LocationDirectionalStimulusActionMap :: Type
type LocationDirectionalStimulusActionMap = Map (GID LocationDirectionalStimulusActionF) LocationDirectionalStimulusActionF

type ObjectDirectionalStimulusActionMap :: Type
type ObjectDirectionalStimulusActionMap = Map (GID ObjectDirectionalStimulusActionF) ObjectDirectionalStimulusActionF

type ConsumptionActionMap :: Type
type ConsumptionActionMap = Map (GID ConsumptionActionF) ConsumptionActionF

type PosturalActionMap :: Type
type PosturalActionMap = Map (GID PosturalActionF) PosturalActionF

type ActionMaps :: Type
data ActionMaps = ActionMaps
  { _agentImplicitStimulusActionMap :: AgentImplicitStimulusActionMap
  , _locationImplicitStimulusActionMap :: LocationImplicitStimulusActionMap
  , _agentDirectionalStimulusContainerActionMap :: AgentDirectionalStimulusContainerActionMap
  , _containerDirectionalStimulusContainerActionMap :: ContainerDirectionalStimulusContainerActionMap
  , _locationDirectionalStimulusContainerActionMap :: LocationDirectionalStimulusContainerActionMap
  , _agentContainerAccessActionMap    :: AgentContainerAccessActionMap
  , _locationContainerAccessActionMap :: LocationContainerAccessActionMap
  , _objectContainerAccessActionMap   :: ObjectContainerAccessActionMap
  , _instrumentContainerAccessActionMap :: InstrumentContainerAccessActionMap
  , _containerAccessActionMap     :: ContainerAccessActionMap
  , _somaticStimulusActionMap     :: SomaticStimulusActionMap
  , _agentAcquisitionActionMap    :: AgentAcquisitionActionMap
  , _objectAcquisitionActionMap   :: ObjectAcquisitionActionMap
  , _containerAcquisitionActionMap :: ContainerAcquisitionActionMap
  , _locationAcquisitionActionMap :: LocationAcquisitionActionMap
  , _agentDirectionalStimulusActionMap :: AgentDirectionalStimulusActionMap
  , _locationDirectionalStimulusActionMap :: LocationDirectionalStimulusActionMap
  , _objectDirectionalStimulusActionMap :: ObjectDirectionalStimulusActionMap
  , _consumptionActionMap         :: ConsumptionActionMap
  , _posturalActionMap            :: PosturalActionMap
  }

-- All Possible Actions
type Config :: Type
data Config = Config
  { _actionMaps :: !ActionMaps }

-- Registry and evaluator types from other modules

type Evaluator :: Type
newtype Evaluator = Evaluator
  { _evaluator :: Sentence -> GameComputation Identity () }

type ActionEffectKey :: Type
data ActionEffectKey
  = AgentImplicitStimulusActionKey (GID AgentImplicitStimulusActionF)
  | LocationImplicitStimulusActionKey (GID LocationImplicitStimulusActionF)
  | AgentDirectionalStimulusActionKey (GID AgentDirectionalStimulusActionF)
  | LocationDirectionalStimulusActionKey (GID LocationDirectionalStimulusActionF)
  | ObjectDirectionalStimulusActionKey (GID ObjectDirectionalStimulusActionF)
  | AgentDirectionalStimulusContainerActionKey (GID AgentDirectionalStimulusContainerActionF)
  | ContainerDirectionalStimulusContainerActionKey (GID ContainerDirectionalStimulusContainerActionF)
  | LocationDirectionalStimulusContainerActionKey (GID LocationDirectionalStimulusContainerActionF)
  | SomaticAccessActionKey (GID SomaticAccessActionF)
  | ContainerAccessActionKey (GID ContainerAccessActionF)
  | AgentContainerAccessActionKey (GID AgentContainerAccessActionF)
  | LocationContainerAccessActionKey (GID LocationContainerAccessActionF)
  | ObjectContainerAccessActionKey (GID ObjectContainerAccessActionF)
  -- Role-based acquisition action keys
  | AgentAcquisitionalActionKey (GID AgentAcquisitionActionF)
  | ObjectAcquisitionalActionKey (GID ObjectAcquisitionActionF)
  | ContainerAcquisitionalActionKey (GID ContainerAcquisitionActionF)
  | LocationAcquisitionalActionKey (GID LocationAcquisitionActionF)
  | ConsumptionActionKey (GID ConsumptionActionF)
  | PosturalActionKey (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)

type PlayerKey :: Type
data PlayerKey
  = PlayerKeyLocation (GID Location)
  | PlayerKeyObject (GID Object)
  deriving stock (Show, Eq, Ord)

type TargetEffectKey :: Type
data TargetEffectKey
  = LocationKey (GID Location)
  | ObjectKey (GID Object)
  | PlayerKey PlayerKey
  deriving stock (Show, Eq, Ord)

type SystemEffectKey :: Type
data SystemEffectKey
  = SystemLocationKey (GID Location)
  | SystemObjectKey (GID Object)
  | SystemPlayerKey PlayerKey
  deriving stock (Show, Eq, Ord)

type FieldUpdateOperation :: Type
data FieldUpdateOperation
  = ObjectShortName (GID Object) Text
  | ObjectDescription (GID Object) Text
  | LocationTitle (GID Location) Text
  | PlayerLocation (GID Location)
  deriving stock (Show, Eq, Ord)

type NarrationComputation :: Type
data NarrationComputation
  = StaticNarration Text
  | InventoryNarration
  | LookNarration
  | LookAtNarration (GID Object)
  | LookInNarration (GID Object)
  deriving stock (Show, Eq, Ord)

type Effect :: Type
data Effect
  = ActionManagementEffect ActionManagementOperation ActionGID
  | FieldUpdateEffect FieldUpdateOperation
  | NarrationEffect NarrationComputation
  deriving stock (Show, Eq, Ord)

type SystemEffect :: Type
data SystemEffect
  = PerceptionSystemEffect (GameComputation Identity ())

type ActionEffectMap :: Type
newtype ActionEffectMap = ActionEffectMap
  { _actionEffectMap :: Map TargetEffectKey (Set Effect) }
  deriving stock (Show, Eq, Ord)

type SystemEffectConfig :: Type
data SystemEffectConfig = SystemEffectConfig
  { _systemEffect           :: SystemEffect
  , _systemEffectManagement :: GameComputation Identity ()
  }

type EffectRegistry :: Type
type EffectRegistry = Map ActionEffectKey ActionEffectMap

type SystemEffectRegistry :: Type
type SystemEffectRegistry = Map SystemEffectKey (Map (GID SystemEffect) SystemEffectConfig)

type SystemEffectKeysRegistry :: Type
type SystemEffectKeysRegistry = Map ActionEffectKey [SystemEffectKey]

type SystemEffectMap :: Type
type SystemEffectMap = Map (GID SystemEffect) SystemEffect

type TriggerRegistry :: Type
newtype TriggerRegistry = TriggerRegistry
  { _unTriggerRegistry :: Map ActionEffectKey [(SystemEffectKey, GID SystemEffect, SystemEffectConfig)] }

type ActionKeyMap :: Type
newtype ActionKeyMap = ActionKeyMap
  { _unActionKeyMap :: Map ActionEffectKey ActionEffectMap }
  deriving stock (Show, Eq, Ord)

type ActionManagementOperation :: Type
data ActionManagementOperation
  = AddAgentImplicitStimulus ImplicitStimulusVerb (GID AgentImplicitStimulusActionF)
  | AddLocationImplicitStimulus ImplicitStimulusVerb (GID LocationImplicitStimulusActionF)
  | AddAgentDirectionalStimulus DirectionalStimulusVerb (GID AgentDirectionalStimulusActionF)
  | AddLocationDirectionalStimulus DirectionalStimulusVerb (GID LocationDirectionalStimulusActionF)
  | AddObjectDirectionalStimulus DirectionalStimulusVerb (GID ObjectDirectionalStimulusActionF)

  | AddAgentDirectionalContainerStimulus DirectionalStimulusVerb (GID AgentDirectionalStimulusContainerActionF)
  | AddContainerDirectionalContainerStimulus DirectionalStimulusVerb (GID ContainerDirectionalStimulusContainerActionF)
  | AddLocationDirectionalContainerStimulus DirectionalStimulusVerb (GID LocationDirectionalStimulusContainerActionF)

  | AddSomaticAccess SomaticAccessVerb (GID SomaticAccessActionF)
  | AddContainerAccess ContainerAccessVerbPhrase (GID ContainerAccessActionF)

  | AddAgentContainerAccessVerbPhrase ContainerAccessVerbPhrase (GID AgentContainerAccessActionF)
  | AddAgentContainerAccessSimpleVerb SimpleAccessVerb (GID AgentContainerAccessActionF)
  | AddLocationContainerAccessVerbPhrase ContainerAccessVerbPhrase (GID LocationContainerAccessActionF)
  | AddLocationContainerAccessSimpleVerb SimpleAccessVerb (GID LocationContainerAccessActionF)
  | AddObjectContainerAccessSimpleVerb SimpleAccessVerb (GID ObjectContainerAccessActionF)
  | AddObjectContainerAccessVerbPhrase ContainerAccessVerbPhrase (GID ObjectContainerAccessActionF)
  | AddInstrumentContainerAccessVerbPhrase ContainerAccessVerbPhrase (GID InstrumentContainerAccessActionF)
  | AddInstrumentContainerAccessSimpleVerb SimpleAccessVerb (GID InstrumentContainerAccessActionF)

  | AddContainerAccessVerb SimpleAccessVerb (GID ContainerAccessActionF)
  | AddAgentAcquisitionVerb AcquisitionVerb (GID AgentAcquisitionActionF)
  | AddObjectAcquisitionVerb AcquisitionVerb (GID ObjectAcquisitionActionF)
  | AddContainerAcquisitionVerb AcquisitionVerb (GID ContainerAcquisitionActionF)
  | AddLocationAcquisitionVerb AcquisitionVerb (GID LocationAcquisitionActionF)

  | AddAgentAcquisitionVerbPhrase AcquisitionVerbPhrase (GID AgentAcquisitionActionF)
  | AddObjectAcquisitionVerbPhrase AcquisitionVerbPhrase (GID ObjectAcquisitionActionF)
  | AddContainerAcquisitionVerbPhrase AcquisitionVerbPhrase (GID ContainerAcquisitionActionF)
  | AddLocationAcquisitionVerbPhrase AcquisitionVerbPhrase (GID LocationAcquisitionActionF)

  | AddConsumption ConsumptionVerb (GID Object) (GID ConsumptionActionF)
  | AddPositivePostural PositivePosturalVerb (GID PosturalActionF)
  | AddNegativePostural NegativePosturalVerb (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)

type ActionGID :: Type
data ActionGID
  = AgentImplicitActionGID (GID AgentImplicitStimulusActionF)
  | LocationImplicitActionGID (GID LocationImplicitStimulusActionF)
  | SomaticAccessActionGID (GID SomaticAccessActionF)
  -- Role-based acquisition action GIDs
  | AgentDirectionalActionGID (GID AgentDirectionalStimulusActionF)
  | LocationDirectionalActionGID (GID LocationDirectionalStimulusActionF)
  | ObjectDirectionalActionGID (GID ObjectDirectionalStimulusActionF)

  | AgentAcquisitionActionGID (GID AgentAcquisitionActionF)
  | ContainerAcquisitionActionGID (GID ContainerAcquisitionActionF)
  | LocationAcquisitionActionGID (GID LocationAcquisitionActionF)

  | ObjectAcquisitionActionGID (GID ObjectAcquisitionActionF)

  | AgentDirectionalContainerActionGID (GID AgentDirectionalStimulusContainerActionF)
  | ContainerDirectionalContainerActionGID (GID ContainerDirectionalStimulusContainerActionF)
  | LocationDirectionalContainerActionGID (GID LocationDirectionalStimulusContainerActionF)

  | ConsumptionActionGID (GID ConsumptionActionF)

  | ContainerAccessActionGID (GID ContainerAccessActionF)
  | AgentContainerAccessActionGID (GID AgentContainerAccessActionF)
  | LocationContainerAccessActionGID (GID LocationContainerAccessActionF)
  | ObjectContainerAccessActionGID (GID ObjectContainerAccessActionF)
  | InstrumentContainerAccessActionGID (GID InstrumentContainerAccessActionF)

  | PosturalActionGID (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)
-- AddAgentAcquisitionVerb
type ActionManagement :: Type
data ActionManagement
  = AgentDSAManagementKey DirectionalStimulusVerb (GID AgentDirectionalStimulusActionF)
  | LocationDSAManagementKey DirectionalStimulusVerb (GID LocationDirectionalStimulusActionF)
  | ObjectDSAManagementKey DirectionalStimulusVerb (GID ObjectDirectionalStimulusActionF)

  | AgentDSAContainerManagementKey DirectionalStimulusVerb (GID AgentDirectionalStimulusContainerActionF)
  | ContainerDSAContainerManagementKey DirectionalStimulusVerb (GID ContainerDirectionalStimulusContainerActionF)
  | LocationDSAContainerManagementKey DirectionalStimulusVerb (GID LocationDirectionalStimulusContainerActionF)

  | AgentISAManagementKey ImplicitStimulusVerb (GID AgentImplicitStimulusActionF)
  | LocationISAManagementKey ImplicitStimulusVerb (GID LocationImplicitStimulusActionF)

  | SSAManagementKey SomaticAccessVerb (GID SomaticAccessActionF)
  -- Role-based acquisition action management keys
  | AgentAVManagementKey AcquisitionVerb (GID AgentAcquisitionActionF)
  | ObjectAVManagementKey AcquisitionVerb (GID ObjectAcquisitionActionF)
  | ContainerAVManagementKey AcquisitionVerb (GID ContainerAcquisitionActionF)
  | LocationAVManagementKey AcquisitionVerb (GID LocationAcquisitionActionF)

  | AgentAAManagementKey AcquisitionVerbPhrase (GID AgentAcquisitionActionF)
  | ObjectAAManagementKey AcquisitionVerbPhrase (GID ObjectAcquisitionActionF)
  | ContainerAAManagementKey AcquisitionVerbPhrase (GID ContainerAcquisitionActionF)
  | LocationAAManagementKey AcquisitionVerbPhrase (GID LocationAcquisitionActionF)

  | CAManagementKey ConsumptionVerb (GID ConsumptionActionF)
  | CVManagementKey ConsumptionVerbPhrase (GID ConsumptionActionF)
  | SAConManagementKey SimpleAccessVerb (GID ContainerAccessActionF)

  | AgentSAConManagementKey SimpleAccessVerb (GID AgentContainerAccessActionF)
  | LocationSAConManagementKey SimpleAccessVerb (GID LocationContainerAccessActionF)
  | ObjectSAConManagementKey SimpleAccessVerb (GID ObjectContainerAccessActionF)
  | InstrumentSAConManagementKey SimpleAccessVerb (GID InstrumentContainerAccessActionF)

  | AgentConManagementKey ContainerAccessVerbPhrase (GID AgentContainerAccessActionF)
  | InstrumentConManagementKey ContainerAccessVerbPhrase (GID InstrumentContainerAccessActionF)
  | LocationConManagementKey ContainerAccessVerbPhrase (GID LocationContainerAccessActionF)
  | ObjectConManagementKey ContainerAccessVerbPhrase (GID ObjectContainerAccessActionF)

  | CONManagementKey ContainerAccessVerbPhrase (GID ContainerAccessActionF)
  | PPManagementKey PositivePosturalVerb (GID PosturalActionF)
  | NPManagementKey NegativePosturalVerb (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)

type ActionManagementFunctions :: Type
newtype ActionManagementFunctions = ActionManagementFunctions
  { _actionManagementFunctions :: Set ActionManagement }
  deriving stock (Show, Eq, Ord)

type SpatialRelationshipMap :: Type
newtype SpatialRelationshipMap = SpatialRelationshipMap
  { _unSpatialRelationshipMap :: Map (GID Object) (Set SpatialRelationship) }
  deriving stock (Show, Eq, Ord)

type SpatialRelationship :: Type
data SpatialRelationship
  = ContainedIn (GID Object)
  | Contains (Set (GID Object))
  | Supports (Set (GID Object))
  | SupportedBy (GID Object)
  | Inventory
  deriving stock (Show, Eq, Ord)

type Perceptables :: Type
newtype Perceptables = Perceptables
  { _perceptables :: Set (GID Object) }
  deriving stock (Show,Eq,Ord)

-- Entity Types
type Player :: Type
data Player = Player
  { _location      :: GID Location
  , _inventory     :: Set (GID Object)
  , _playerActions :: ActionManagementFunctions
  }
  deriving stock (Show, Eq, Ord)

type Location :: Type
data Location = Location {
    _title                    :: Text
  , _objectSemanticMap        :: Map NounKey (Set (GID Object))
  , _locationInventory        :: Set (GID Object)
  , _locationActionManagement :: ActionManagementFunctions
}
  deriving stock (Show, Eq, Ord)

type Object :: Type
data Object = Object
 { _shortName              :: Text
 , _description            :: Text
 , _descriptives           :: Set DirectionalStimulusNounPhrase
 , _objectActionManagement :: ActionManagementFunctions
 }
  deriving stock (Show, Eq, Ord)

type World :: Type
data World = World
  { _objectMap              :: GIDToDataMap Object Object
  , _locationMap            :: GIDToDataMap Location Location
  , _perceptionMap          :: Map DirectionalStimulusNounPhrase (Set (GID Object))
  , _spatialRelationshipMap :: SpatialRelationshipMap
  , _globalSemanticMap      :: Map NounKey (Set (GID Object))
  }
  deriving stock (Show, Eq, Ord)

type Narration :: Type
data Narration = Narration
  { _playerAction      :: [Text]
  , _actionConsequence :: [Text]
  }
  deriving stock (Show, Eq, Ord)

type GameState :: Type
data GameState = GameState
  { _world                  :: World
  , _player                 :: Player
  , _narration              :: Narration
  , _evaluation             :: Evaluator
  , _effectRegistry         :: EffectRegistry
  , _actionSystemEffectKeys :: SystemEffectKeysRegistry
  , _triggerRegistry        :: TriggerRegistry
  , _systemEffectRegistry   :: SystemEffectRegistry
  }

#ifdef TESTING

-- Simple Arbitrary instance for GID (just use Int)
instance Arbitrary a => Arbitrary (GID a) where
  arbitrary = GID <$> choose (1, 1000)

-- Small Text generator for testing
newtype SmallText = SmallText Text
instance Arbitrary SmallText where
  arbitrary = SmallText . pack <$> resize 20 (listOf (choose ('a', 'z')))

-- Helper for arbitrary text
arbitraryText :: Gen Text
arbitraryText = pack <$> resize 15 (listOf (choose ('a', 'z')))

-- Basic data type instances
instance Arbitrary Location where
  arbitrary = Location <$> arbitraryText <*> pure mempty <*> pure mempty <*> pure (ActionManagementFunctions mempty)

instance Arbitrary Object where
  arbitrary = Object <$> arbitraryText <*> arbitraryText <*> pure mempty <*> pure (ActionManagementFunctions mempty)

-- TargetEffectKey instances
instance Arbitrary TargetEffectKey where
  arbitrary = oneof
    [ LocationKey <$> arbitrary
    , ObjectKey <$> arbitrary
    , PlayerKey <$> arbitrary
    ]

-- PlayerKey instances
instance Arbitrary PlayerKey where
  arbitrary = oneof
    [ PlayerKeyLocation <$> arbitrary
    , PlayerKeyObject <$> arbitrary
    ]

-- ActionEffectKey instances (using simple GID generation, not actual functions)
instance Arbitrary ActionEffectKey where
  arbitrary = oneof
    [
     SomaticAccessActionKey . GID <$> choose (1, 1000)
    , ContainerAccessActionKey . GID <$> choose (1, 1000)
    , AgentAcquisitionalActionKey . GID <$> choose (1, 1000)
    , ConsumptionActionKey . GID <$> choose (1, 1000)
    , PosturalActionKey . GID <$> choose (1, 1000)
    ]

-- FieldUpdateOperation instances
instance Arbitrary FieldUpdateOperation where
  arbitrary = oneof
    [ ObjectShortName <$> arbitrary <*> arbitraryText
    , ObjectDescription <$> arbitrary <*> arbitraryText
    , LocationTitle <$> arbitrary <*> arbitraryText
    , PlayerLocation <$> arbitrary
    ]

-- Effect instances (focusing on FieldUpdateEffect, skipping function types)
instance Arbitrary Effect where
  arbitrary = FieldUpdateEffect <$> arbitrary

-- ActionEffectMap instances
instance Arbitrary ActionEffectMap where
  arbitrary = ActionEffectMap <$> arbitrary

#endif
