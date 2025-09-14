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
  , ImplicitStimulusActionF(..)
  , DirectionalStimulusActionF(..)
  , DirectionalStimulusContainerActionF(..)
  , ContainerAccessActionF(..)
  , SomaticAccessActionF(..)
  , PosturalActionF(..)
  , AcquisitionActionF(..)
  , ConsumptionActionF(..)
    -- * Action Maps
  , ActionMaps(..)
  , ActionEffectMap(..)
  , ImplicitStimulusActionMap
  , DirectionalStimulusActionMap
  , DirectionalStimulusContainerActionMap
  , ContainerAccessActionMap
  , SomaticAccessActionMap
  , PosturalActionMap
  , AcquisitionVerbActionMap
  , ConsumptionActionMap
    -- * Action Management
  , ActionManagement(..)
  , ActionManagementFunctions(..)
  , ActionGID(..)
  , ActionManagementOperation(..)
    -- * Processing Types
  , ProcessImplicitStimulusVerb(..)
  , ProcessDirectionalStimulusVerb(..)
  , ProcessImplicitVerbMap
  , ProcessImplicitVerbMaps
  , PlayerProcessImplicitVerbMap
  , SystemEffectMap
    -- * Search and Access Types
  , SimpleAccessSearchStrategy
  , SearchStrategy
  , FinalizeAccessNotInstrumentF
  , ContainerAccessF
  , AcquisitionF
  , FinalizeAcquisitionF
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
  , ContainerAccessResult(..)
  , CoordinationResult(..)
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
                                                DirectionalStimulusNounPhrase)
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
type ImplicitStimulusActionF :: Type
data ImplicitStimulusActionF
  = PlayerImplicitStimulusActionF (ActionEffectKey -> GameComputation Identity ())
  | CannotImplicitStimulusActionF (ActionEffectKey -> GameComputation Identity ())

type DirectionalStimulusActionF :: Type
data DirectionalStimulusActionF
  = PlayerDirectionalStimulusActionF (ActionEffectKey -> DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ())
  | ObjectDirectionalStimulusActionF (ActionEffectKey -> GameComputation Identity ())
  | CannotSeeF (ActionEffectKey -> GameComputation Identity ())

type DirectionalStimulusContainerActionF :: Type
data DirectionalStimulusContainerActionF
  = PlayerDirectionalStimulusContainerActionF (ActionEffectKey -> DirectionalStimulusVerb -> ContainerPhrase -> GameComputation Identity ())
  | ObjectDirectionalStimulusContainerActionF (ActionEffectKey -> GameComputation Identity ())
  | CannotSeeInF (ActionEffectKey -> GameComputation Identity ())

type SimpleAccessSearchStrategy :: Type
type SimpleAccessSearchStrategy = NounKey
                                    -> GameComputation Identity (Maybe (GID Object))

type ContainerAccessResult :: Type
data ContainerAccessResult = ContainerAccessResult
  { _containerActionEffectKeys :: [ActionEffectKey]
  , _containerFieldEffectKeys  :: [ActionEffectKey]
  }
  deriving stock (Show, Eq, Ord)

type InstrumentAccessResult :: Type
data InstrumentAccessResult = InstrumentAccessResult
  { _instrumentActionEffectKeys :: [ActionEffectKey]
  , _instrumentFieldEffectKeys  :: [ActionEffectKey]
  }
  deriving stock (Show, Eq, Ord)

type FinalizeAccessNotInstrumentF :: Type
type FinalizeAccessNotInstrumentF = ActionEffectKey
                                      -> GameComputation Identity ContainerAccessResult
                                      -> GameComputation Identity ()

type ContainerAccessF :: Type
type ContainerAccessF = (ActionEffectKey
                           -> SimpleAccessSearchStrategy
                           -> ContainerAccessActionMap
                           -> ContainerAccessVerbPhrase
                           -> FinalizeAccessNotInstrumentF
                           -> GameComputation Identity ())

type ContainerAccessActionF :: Type
data ContainerAccessActionF
  = PlayerContainerAccessF ContainerAccessF
  | ObjectContainerAccessF (GameComputation Identity ContainerAccessResult)
  | InstrumentContainerAccessF (GID Object -> GameComputation Identity InstrumentAccessResult)
  | CannotAccessF          (GameComputation Identity ())

type SomaticAccessActionF :: Type
data SomaticAccessActionF
  = PlayerSomaticAccessActionF (Set TargetEffectKey -> [SystemEffectKey] -> ActionEffectMap -> SystemEffectRegistry -> GameComputation Identity ())
  | CannotSomaticAccessF (GameComputation Identity ())

type PosturalActionF :: Type
data PosturalActionF
  = PlayerPosturalActionF (Set TargetEffectKey -> ActionEffectMap -> GameComputation Identity ())
  | CannotPosturalActionF (GameComputation Identity ())

type CoordinationResult :: Type
data CoordinationResult = CoordinationResult
  { _computation      :: GameComputation Identity ()
  , _actionEffectKeys :: [ActionEffectKey]
  , _fieldEffectKeys  :: [ActionEffectKey]
  }

type SearchStrategy :: Type
type SearchStrategy = NounKey
                        -> GameComputation Identity (Maybe (GID Object, GID Object))

type AcquisitionF :: Type
type AcquisitionF = (ActionEffectKey -> AcquisitionVerbActionMap -> SearchStrategy -> AcquisitionVerbPhrase -> FinalizeAcquisitionF -> GameComputation Identity ())

type FinalizeAcquisitionF :: Type
type FinalizeAcquisitionF = ActionEffectKey
                              -> GID Object
                              -> GID Object
                              -> GameComputation Identity CoordinationResult
                              -> (GID Object -> GameComputation Identity CoordinationResult)
                              -> GameComputation Identity ()

type AcquisitionActionF :: Type
data AcquisitionActionF
  = AcquisitionActionF AcquisitionF
  | CollectedF (GameComputation Identity CoordinationResult)
  | LosesObjectF (GID Object -> GameComputation Identity CoordinationResult)
  | NotGettableF (GameComputation Identity ())

type ConsumptionActionF :: Type
data ConsumptionActionF
  = PlayerConsumptionActionF (GID Object -> Set TargetEffectKey -> ActionEffectMap -> ConsumptionVerbPhrase -> GameComputation Identity ())
  | CannotConsumeF (GameComputation Identity ())

type ProcessImplicitStimulusVerb :: Type
newtype ProcessImplicitStimulusVerb = ProcessImplicitStimulusVerb
  { _unProcessImplicitStimlusVerb :: ImplicitStimulusVerb -> GameComputation Identity ()}

type ProcessDirectionalStimulusVerb :: Type
newtype ProcessDirectionalStimulusVerb = ProcessDirectionalStimulusVerb
  { _unProcessDirectionalStimlusVerb :: DirectionalStimulusVerb
                                          -> DirectionalStimulusNounPhrase
                                          -> GameComputation Identity ()
  }

type ProcessImplicitVerbMap :: Type
type ProcessImplicitVerbMap = Map (GID ProcessImplicitStimulusVerb) (ImplicitStimulusVerb -> ImplicitStimulusActionF)

type ProcessImplicitVerbMaps :: Type
type ProcessImplicitVerbMaps = Map ImplicitStimulusVerb ProcessImplicitVerbMap

type PlayerProcessImplicitVerbMap :: Type
type PlayerProcessImplicitVerbMap = Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb)

-- Action Maps
type ImplicitStimulusActionMap :: Type
type ImplicitStimulusActionMap = Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF

type DirectionalStimulusActionMap :: Type
type DirectionalStimulusActionMap = Map (GID DirectionalStimulusActionF) DirectionalStimulusActionF

type DirectionalStimulusContainerActionMap :: Type
type DirectionalStimulusContainerActionMap = Map (GID DirectionalStimulusContainerActionF) DirectionalStimulusContainerActionF

type ContainerAccessActionMap :: Type
type ContainerAccessActionMap = Map (GID ContainerAccessActionF) ContainerAccessActionF

type SomaticAccessActionMap :: Type
type SomaticAccessActionMap = Map (GID SomaticAccessActionF) SomaticAccessActionF

type SomaticStimulusActionMap :: Type
type SomaticStimulusActionMap = Map (GID SomaticAccessActionF) SomaticAccessActionF

type AcquisitionVerbActionMap :: Type
type AcquisitionVerbActionMap = Map (GID AcquisitionActionF) AcquisitionActionF

type ConsumptionActionMap :: Type
type ConsumptionActionMap = Map (GID ConsumptionActionF) ConsumptionActionF

type PosturalActionMap :: Type
type PosturalActionMap = Map (GID PosturalActionF) PosturalActionF

type ActionMaps :: Type
data ActionMaps = ActionMaps
  { _implicitStimulusActionMap    :: ImplicitStimulusActionMap
  , _directionalStimulusActionMap :: DirectionalStimulusActionMap
  , _directionalStimulusContainerActionMap :: DirectionalStimulusContainerActionMap
  , _containerAccessActionMap     :: ContainerAccessActionMap
  , _somaticStimulusActionMap     :: SomaticStimulusActionMap
  , _acquisitionActionMap         :: AcquisitionVerbActionMap
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
  = ImplicitStimulusActionKey (GID ImplicitStimulusActionF)
  | DirectionalStimulusActionKey (GID DirectionalStimulusActionF)
  | DirectionalStimulusContainerActionKey (GID DirectionalStimulusContainerActionF)
  | SomaticAccessActionKey (GID SomaticAccessActionF)
  | ContainerAccessActionKey (GID ContainerAccessActionF)
  | AcquisitionalActionKey (GID AcquisitionActionF)
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

type Effect :: Type
data Effect
  = ActionManagementEffect ActionManagementOperation ActionGID
  | FieldUpdateEffect FieldUpdateOperation
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

-- Registry types

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
  = AddImplicitStimulus ImplicitStimulusVerb (GID ImplicitStimulusActionF)
  | AddDirectionalStimulus DirectionalStimulusVerb (GID DirectionalStimulusActionF)
  | AddDirectionalContainerStimulus DirectionalStimulusVerb (GID DirectionalStimulusContainerActionF)
  | AddSomaticAccess SomaticAccessVerb (GID SomaticAccessActionF)
  | AddAcquisitionVerb AcquisitionVerb (GID AcquisitionActionF)
  | AddContainerAccess ContainerAccessVerbPhrase (GID ContainerAccessActionF)
  | AddContainerAccessVerb SimpleAccessVerb (GID ContainerAccessActionF)
  | AddAcquisitionVerbPhrase AcquisitionVerbPhrase (GID AcquisitionActionF)
  | AddConsumption ConsumptionVerb (GID Object) (GID ConsumptionActionF)
  | AddPositivePostural PositivePosturalVerb (GID PosturalActionF)
  | AddNegativePostural NegativePosturalVerb (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)

type ActionGID :: Type
data ActionGID
  = ImplicitActionGID (GID ImplicitStimulusActionF)
  | DirectionalActionGID (GID DirectionalStimulusActionF)
  | DirectionalContainerActionGID (GID DirectionalStimulusContainerActionF)
  | SomaticAccessActionGID (GID SomaticAccessActionF)
  | AcquisitionActionGID (GID AcquisitionActionF)
  | ConsumptionActionGID (GID ConsumptionActionF)
  | ContainerAccessActionGID (GID ContainerAccessActionF)
  | PosturalActionGID (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)

type ActionManagement :: Type
data ActionManagement
  = DSAManagementKey DirectionalStimulusVerb (GID DirectionalStimulusActionF)
  | DSAContainerManagementKey DirectionalStimulusVerb (GID DirectionalStimulusContainerActionF)
  | ISAManagementKey ImplicitStimulusVerb (GID ImplicitStimulusActionF)
  | SSAManagementKey SomaticAccessVerb (GID SomaticAccessActionF)
  | AVManagementKey AcquisitionVerb (GID AcquisitionActionF)
  | AAManagementKey AcquisitionVerbPhrase (GID AcquisitionActionF)
  | CAManagementKey ConsumptionVerb (GID ConsumptionActionF)
  | CVManagementKey ConsumptionVerbPhrase (GID ConsumptionActionF)
  | SAConManagementKey SimpleAccessVerb (GID ContainerAccessActionF)
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
  arbitrary = Location <$> arbitraryText <*> pure mempty <*> pure (ActionManagementFunctions mempty)

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
    [ ImplicitStimulusActionKey . GID <$> choose (1, 1000)
    , DirectionalStimulusActionKey . GID <$> choose (1, 1000)
    , DirectionalStimulusContainerActionKey . GID <$> choose (1, 1000)
    , SomaticAccessActionKey . GID <$> choose (1, 1000)
    , ContainerAccessActionKey . GID <$> choose (1, 1000)
    , AcquisitionalActionKey . GID <$> choose (1, 1000)
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
