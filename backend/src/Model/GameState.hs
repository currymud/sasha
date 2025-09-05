{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.GameState (
   ActionEffectKey (LocationKey, ObjectKey, PlayerKey)
  , ActionEffectMap (ActionEffectMap, _actionEffectMap)
  , ActionGID (ImplicitActionGID, DirectionalActionGID, DirectionalContainerActionGID , SomaticAccessActionGID,
               AcquisitionActionGID, ConsumptionActionGID, PosturalActionGID, ContainerAccessActionGID)
  , AcquisitionF
  , AcquisitionRes (Complete, Simple)
  , AcquisitionVerbActionMap
  , ActionManagementOperation (AddImplicitStimulus, AddDirectionalStimulus, AddDirectionalContainerStimulus, AddSomaticAccess,
                               AddAcquisitionVerb, AddAcquisitionVerbPhrase ,AddConsumption,
                               AddPositivePostural, AddNegativePostural, AddContainerAccess,AddContainerAccessVerb)
  , SimpleAcquisitionRes (SimpleAcquisitionRes, _saObjectKey, _saObjectPhrase)
  , CompleteAcquisitionRes (CompleteAcquisitionRes, _caObjectKey, _caObjectPhrase, _caSupportKey, _caSupportPhrase)
  , AccessRes (CompleteAR, SimpleAR)
  , ActionKeyMap (ActionKeyMap, _unActionKeyMap)
  , ActionManagement (DSAContainerManagementKey, DSAManagementKey, ISAManagementKey, SSAManagementKey,
                      AAManagementKey,AVManagementKey, CAManagementKey,CVManagementKey,CONManagementKey,SAConManagementKey,
                      PPManagementKey, NPManagementKey)
  , ActionManagementFunctions (ActionManagementFunctions, _actionManagementFunctions)
  , ActionMaps (ActionMaps
                 , _acquisitionActionMap
                 , _implicitStimulusActionMap
                 , _directionalStimulusActionMap
                 , _directionalStimulusContainerActionMap
                 , _containerAccessActionMap
                 , _posturalActionMap
                 ,_somaticStimulusActionMap
                 ,_acquisitionActionMap
                 , _consumptionActionMap)
  , AcquisitionActionF (AcquisitionActionF,CollectedF,LosesObjectF,NotGettableF)
  , ConsumptionActionF (ConsumptionActionF, _consumptionAction)
  , ConsumptionActionMap
  , ContainerAccessF
  , ContainerAccessActionMap
  , ContainerAccessResult (ContainerAccessResult, _containerActionEffectKeys, _containerFieldEffectKeys)
  , Config (Config, _actionMaps)
  , CompleteAccessRes (CompleteAccessRes, _containerKey, _ContainerPhrase, _instrumentKey, _instrumentPhrase)
  , SimpleAccessRes (SimpleAccessRes,  _saContainerKey, _saContainerPhrase)
  , ContainerAccessActionF (PlayerContainerAccessF,ObjectContainerAccessF,CannotAccessF,InstrumentContainerAccessF)
  , CoordinationResult (CoordinationResult, _computation, _actionEffectKeys, _fieldEffectKeys)
  , DirectionalStimulusActionF (PlayerDirectionalStimulusActionF,ObjectDirectionalStimulusActionF,CannotSeeF)
  , DirectionalStimulusActionMap
  , DirectionalStimulusContainerActionF (PlayerDirectionalStimulusContainerActionF,ObjectDirectionalStimulusContainerActionF,CannotSeeInF)
  , DirectionalStimulusContainerActionMap
  , DisplayT (DisplayT, runDisplayT)
  , Effect (ActionManagementEffect, FieldUpdateEffect)
  , EffectActionKey ( ImplicitStimulusActionKey
                      , DirectionalStimulusActionKey
                      , DirectionalStimulusContainerActionKey
                      , ContainerAccessActionKey
                      , SomaticAccessActionKey
                      , AcquisitionalActionKey
                      , ConsumptionActionKey
                      , PosturalActionKey)
  , EffectRegistry
  , Evaluator
  , FieldUpdateOperation (ObjectShortName, ObjectDescription, LocationTitle, PlayerLocation)
  , FinalizeAcquisitionF
  , FinalizeAccessNotInstrumentF
  , GameComputation (GameComputation, runGameComputation)
  , GameState (GameState, _triggerRegistry, _systemEffectRegistry , _world, _player, _narration, _evaluation, _effectRegistry,_actionSystemEffectKeys)
  , GameStateT (GameStateT, runGameStateT)
  , GameT (GameT, runGameT)
  , ImplicitStimulusActionF (ImplicitStimulusActionF, _implicitStimulusAction)
  , ImplicitStimulusActionMap
  , InstrumentAccessResult (InstrumentAccessResult, _instrumentActionEffectKeys, _instrumentFieldEffectKeys)
  , Location (Location, _title, _objectSemanticMap, _locationActionManagement)
  , Narration (Narration, _playerAction, _actionConsequence)
  , Object (Object, _shortName, _description, _descriptives, _objectActionManagement)
  , transformToIO, liftDisplay
  , fromDisplay
  , Perceptables (Perceptables, _perceptables)
  , Player (Player, _location, _playerActions)
  , PlayerKey (PlayerKeyLocation, PlayerKeyObject)
  , PlayerProcessImplicitVerbMap
  , PosturalActionF (PosturalActionF)
  , PosturalActionMap
  , ProcessDirectionalStimulusVerb (ProcessDirectionalStimulusVerb, _unProcessDirectionalStimlusVerb)
  , ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb, _unProcessImplicitStimlusVerb)
  , ProcessImplicitVerbMap
  , ProcessImplicitVerbMaps
  , SearchStrategy
  , SimpleAccessSearchStrategy
  , SomaticAccessActionF (SomaticAccessActionF, _somaticAccessAction)
  , SomaticStimulusActionMap
  , SpatialRelationship (ContainedIn, Contains, Inventory, Supports, SupportedBy)
  , SpatialRelationshipMap (SpatialRelationshipMap, _spatialRelationshipMap)
  , SystemEffect (PerceptionSystemEffect)
  , SystemEffectConfig (SystemEffectConfig, _systemEffect, _systemEffectManagement)
  , SystemEffectKey (SystemLocationKey, SystemObjectKey, SystemPlayerKey)
  , SystemEffectKeysRegistry
  , SystemEffectMap
  , SystemEffectRegistry
  , TriggerRegistry
  , World (World, _objectMap, _locationMap,_perceptionMap, _spatialRelationshipMap)
  , liftToDisplay
  , updateActionConsequence
  , updatePlayerAction) where

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
import           Model.GameState.Mappings      (GIDToDataMap)
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

-- Game Transformers
type GameStateT :: (Type -> Type) -> Type -> Type
newtype GameStateT m a = GameStateT {runGameStateT :: StateT GameState m a}
  deriving newtype ( Functor
                   , Applicative
                   , MFunctor
                   , Monad
                   , MonadState GameState,MonadIO)

instance MonadTrans GameStateT where
  lift = GameStateT . lift

type GameComputation :: (Type -> Type) -> Type -> Type
newtype GameComputation m a = GameComputation
  { runGameComputation :: ReaderT Config (ExceptT Text (GameStateT m)) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Config, MonadError Text, MonadState GameState
    )

instance MonadTrans GameComputation where
  lift = GameComputation . lift . lift . lift

type GameT :: (Type -> Type) -> Type -> Type
newtype GameT m a = GameT
  { runGameT :: ReaderT Config (ExceptT Text (GameStateT m)) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Config, MonadError Text, MonadState GameState, MonadIO
    )

instance MonadTrans GameT where
  lift = GameT . lift . lift . lift

type DisplayT :: (Type -> Type) -> Type -> Type
newtype DisplayT m a = DisplayT { runDisplayT :: GameStateT m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadState GameState)

instance MonadTrans DisplayT where
  lift = DisplayT . lift

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

type ImplicitStimulusActionMap :: Type
type ImplicitStimulusActionMap = Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF

type ImplicitStimulusActionF :: Type
newtype ImplicitStimulusActionF
  = ImplicitStimulusActionF  { _implicitStimulusAction :: Player
                                                            -> Location
                                                            -> GameComputation Identity () }

type DirectionalStimulusActionMap :: Type
type DirectionalStimulusActionMap = Map (GID DirectionalStimulusActionF) DirectionalStimulusActionF

type DirectionalStimulusActionF :: Type
data DirectionalStimulusActionF
  = PlayerDirectionalStimulusActionF (DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ())
  | ObjectDirectionalStimulusActionF (GameComputation Identity ())
  | CannotSeeF (GameComputation Identity ())

type DirectionalStimulusContainerActionF :: Type
data DirectionalStimulusContainerActionF
  = PlayerDirectionalStimulusContainerActionF (DirectionalStimulusVerb -> ContainerPhrase -> GameComputation Identity ())
  | ObjectDirectionalStimulusContainerActionF (GameComputation Identity ())
  | CannotSeeInF (GameComputation Identity ())

type ContainerAccessF :: Type
type ContainerAccessF = (EffectActionKey
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

type FinalizeAccessNotInstrumentF :: Type
type FinalizeAccessNotInstrumentF = EffectActionKey
                                      -> GameComputation Identity ContainerAccessResult
                                      -> GameComputation Identity ()

type ContainerAccessActionMap :: Type
type ContainerAccessActionMap = Map (GID ContainerAccessActionF) ContainerAccessActionF

type DirectionalStimulusContainerActionMap :: Type
type DirectionalStimulusContainerActionMap = Map (GID DirectionalStimulusContainerActionF) DirectionalStimulusContainerActionF

type SomaticStimulusActionMap :: Type
type SomaticStimulusActionMap = Map (GID SomaticAccessActionF) SomaticAccessActionF

type AcquisitionVerbActionMap :: Type
type AcquisitionVerbActionMap = Map (GID AcquisitionActionF) AcquisitionActionF

type ConsumptionActionMap :: Type
type ConsumptionActionMap = Map (GID ConsumptionActionF) ConsumptionActionF

type PosturalActionMap :: Type
type PosturalActionMap = Map (GID PosturalActionF) PosturalActionF

type SomaticAccessActionF :: Type
newtype SomaticAccessActionF = SomaticAccessActionF
  { _somaticAccessAction :: Set ActionEffectKey
                              -> [SystemEffectKey]
                              -> ActionEffectMap
                              -> SystemEffectRegistry
                              -> GameComputation Identity () }

type PosturalActionF :: Type
newtype PosturalActionF = PosturalActionF
  { _positivePosturalAction :: Set ActionEffectKey -> ActionEffectMap -> GameComputation Identity () }

type SearchStrategy :: Type
type SearchStrategy = NounKey
                        -> GameComputation Identity (Maybe (GID Object, GID Object))

type SimpleAccessSearchStrategy :: Type
type SimpleAccessSearchStrategy = NounKey
                                    -> GameComputation Identity (Maybe (GID Object))
type CoordinationResult :: Type
data CoordinationResult = CoordinationResult
  { _computation      :: GameComputation Identity ()
  , _actionEffectKeys :: [EffectActionKey]
  , _fieldEffectKeys  :: [EffectActionKey]
  }

type ContainerAccessResult :: Type
data ContainerAccessResult = ContainerAccessResult
  { _containerActionEffectKeys :: [EffectActionKey]
  , _containerFieldEffectKeys  :: [EffectActionKey]
  }
  deriving stock (Show, Eq, Ord)

type InstrumentAccessResult :: Type
data InstrumentAccessResult = InstrumentAccessResult
  { _instrumentActionEffectKeys :: [EffectActionKey]
  , _instrumentFieldEffectKeys  :: [EffectActionKey]
  }
  deriving stock (Show, Eq, Ord)

type CompleteAccessRes :: Type
data CompleteAccessRes = CompleteAccessRes
  { _containerKey     :: NounKey
  , _ContainerPhrase  :: ContainerPhrase
  , _instrumentKey    :: NounKey
  , _instrumentPhrase :: InstrumentalAccessNounPhrase
  }
  deriving stock (Show, Eq, Ord)

type SimpleAccessRes :: Type
data SimpleAccessRes = SimpleAccessRes
  { _saContainerKey    :: NounKey
  , _saContainerPhrase :: ContainerPhrase
  }
  deriving stock (Show, Eq, Ord)

type AccessRes :: Type
data AccessRes
  = CompleteAR CompleteAccessRes
  | SimpleAR SimpleAccessRes
  deriving stock (Show, Eq, Ord)

type AcquisitionRes :: Type
data AcquisitionRes
  = Complete CompleteAcquisitionRes
  | Simple SimpleAcquisitionRes
  deriving stock (Show, Eq, Ord)

type CompleteAcquisitionRes :: Type
data CompleteAcquisitionRes = CompleteAcquisitionRes
  { _caObjectKey     :: NounKey
  , _caObjectPhrase  :: ObjectPhrase
  , _caSupportKey    :: NounKey
  , _caSupportPhrase :: SupportPhrase
  }
  deriving stock (Show, Eq, Ord)

type SimpleAcquisitionRes :: Type
data SimpleAcquisitionRes = SimpleAcquisitionRes
  { _saObjectKey    :: NounKey
  , _saObjectPhrase :: ObjectPhrase
  }
  deriving stock (Show, Eq, Ord)

type AcquisitionActionF :: Type
data AcquisitionActionF
 = AcquisitionActionF AcquisitionF
 | CollectedF (GameComputation Identity CoordinationResult)
 | LosesObjectF (GID Object -> GameComputation Identity CoordinationResult)
 | NotGettableF (GameComputation Identity ())

type AcquisitionF :: Type
type AcquisitionF = (EffectActionKey -> AcquisitionVerbActionMap -> SearchStrategy -> AcquisitionVerbPhrase -> FinalizeAcquisitionF -> GameComputation Identity ())

type FinalizeAcquisitionF :: Type
type FinalizeAcquisitionF = EffectActionKey
                              -> GID Object
                              -> GID Object
                              -> GameComputation Identity CoordinationResult
                              -> (GID Object -> GameComputation Identity CoordinationResult)
                              -> GameComputation Identity ()
type ConsumptionActionF :: Type
newtype ConsumptionActionF = ConsumptionActionF
  { _consumptionAction :: GID Object
                            -> Set ActionEffectKey
                            -> ActionEffectMap
                            -> ConsumptionVerbPhrase
                            -> GameComputation Identity () }

type ProcessImplicitVerbMap :: Type
type ProcessImplicitVerbMap = Map (GID ProcessImplicitStimulusVerb) (ImplicitStimulusVerb -> ImplicitStimulusActionF)

type ProcessImplicitVerbMaps :: Type
type ProcessImplicitVerbMaps = Map ImplicitStimulusVerb ProcessImplicitVerbMap

type PlayerProcessImplicitVerbMap :: Type
type PlayerProcessImplicitVerbMap = Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb)

type ProcessImplicitStimulusVerb :: Type
newtype ProcessImplicitStimulusVerb = ProcessImplicitStimulusVerb
  { _unProcessImplicitStimlusVerb :: ImplicitStimulusVerb -> GameComputation Identity ()}

type ProcessDirectionalStimulusVerb :: Type
newtype ProcessDirectionalStimulusVerb = ProcessDirectionalStimulusVerb
  { _unProcessDirectionalStimlusVerb :: DirectionalStimulusVerb
                                          -> DirectionalStimulusNounPhrase
                                          -> GameComputation Identity ()
  }

type Evaluator :: Type
type Evaluator
  = (Sentence -> GameComputation Identity ())

type ActionEffectKey :: Type
data ActionEffectKey
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

type PlayerKey :: Type
data PlayerKey
  = PlayerKeyLocation (GID Location)
  | PlayerKeyObject (GID Object)
  deriving stock (Show, Eq, Ord)

type FieldUpdateOperation :: Type
data FieldUpdateOperation
  = ObjectShortName (GID Object) Text
  | ObjectDescription (GID Object) Text
  | LocationTitle (GID Location) Text
  | PlayerLocation (GID Location)
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

type EffectActionKey :: Type
data EffectActionKey
  = ImplicitStimulusActionKey (GID ImplicitStimulusActionF)
  | DirectionalStimulusActionKey (GID DirectionalStimulusActionF)
  | DirectionalStimulusContainerActionKey (GID DirectionalStimulusContainerActionF)
  | SomaticAccessActionKey (GID SomaticAccessActionF)
  | ContainerAccessActionKey (GID ContainerAccessActionF)
  | AcquisitionalActionKey (GID AcquisitionActionF)
  | ConsumptionActionKey (GID ConsumptionActionF)
  | PosturalActionKey (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)

type Effect :: Type
data Effect
  = ActionManagementEffect ActionManagementOperation ActionGID
  | FieldUpdateEffect FieldUpdateOperation
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

type EffectRegistry :: Type
type EffectRegistry = Map EffectActionKey ActionEffectMap

type TriggerRegistry :: Type
type TriggerRegistry = Map EffectActionKey [(SystemEffectKey, GID SystemEffect, SystemEffectConfig)]

type SystemEffectConfig :: Type
data SystemEffectConfig = SystemEffectConfig
  { _systemEffect           :: SystemEffect
  , _systemEffectManagement :: GameComputation Identity ()
  }

-- We're punting SystemEffectRegistry for now - it will be a map from SystemEffectKey to a map of SystemEffect GIDs to SystemEffectConfig
type SystemEffectRegistry :: Type
type SystemEffectRegistry = Map SystemEffectKey (Map (GID SystemEffect) SystemEffectConfig)

type SystemEffectMap :: Type
type SystemEffectMap = Map (GID SystemEffect) SystemEffect


type SystemEffect :: Type
data SystemEffect
  = PerceptionSystemEffect (GameComputation Identity ())

type ActionEffectMap :: Type
newtype ActionEffectMap = ActionEffectMap
  { _actionEffectMap :: Map ActionEffectKey (Set Effect)}
  deriving stock (Show, Eq, Ord)

type ActionKeyMap :: Type
newtype ActionKeyMap = ActionKeyMap
  { _unActionKeyMap :: Map EffectActionKey ActionEffectMap }
  deriving stock (Show, Eq, Ord)

type Config :: Type
data Config = Config
  { _actionMaps      :: ActionMaps
  }

type SystemEffectKeysRegistry :: Type
type SystemEffectKeysRegistry = Map EffectActionKey [SystemEffectKey]

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


type Location :: Type
data Location = Location {
    _title                    :: Text
  , _objectSemanticMap        :: Map NounKey (Set (GID Object))
  , _locationActionManagement :: ActionManagementFunctions
--  , _locationEffects          :: LocationEffects
}
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

type Narration :: Type
data Narration = Narration
  { _playerAction      :: [Text] -- what player tried to do
  , _actionConsequence :: [Text] -- what happened as a result of the action
  }
  deriving stock (Show)

type Player :: Type
data Player = Player
  { _location      :: GID Location
  , _playerActions :: ActionManagementFunctions
  }
  deriving stock (Show, Eq, Ord)

type SpatialRelationshipMap :: Type
newtype SpatialRelationshipMap = SpatialRelationshipMap
  { _spatialRelationshipMap :: Map (GID Object) (Set SpatialRelationship) }
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

type World :: Type
data World = World
  { _objectMap              :: GIDToDataMap Object Object
  , _locationMap            :: GIDToDataMap Location Location
  , _perceptionMap          :: Map DirectionalStimulusNounPhrase (Set (GID Object))
  , _spatialRelationshipMap :: SpatialRelationshipMap
  }

updatePlayerAction :: Text -> Narration -> Narration
updatePlayerAction action narration =
  narration { _playerAction = action : _playerAction narration }

updateActionConsequence :: Text -> Narration -> Narration
updateActionConsequence consequence narration =
  narration { _actionConsequence = consequence : _actionConsequence narration }

type Object :: Type
data Object = Object
 { _shortName              :: Text
 , _description            :: Text
 , _descriptives           :: Set DirectionalStimulusNounPhrase
 , _objectActionManagement :: ActionManagementFunctions
 }

liftToDisplay :: GameStateT m a -> DisplayT m a
liftToDisplay = DisplayT

fromDisplay :: DisplayT m a -> GameStateT m a
fromDisplay = runDisplayT

identityToIO :: Identity a -> IO a
identityToIO = return . runIdentity

-- | Transform GameComputation Identity to GameT IO - change base monad using hoist
transformToIO :: GameComputation Identity a -> GameT IO a
transformToIO comp = GameT $ hoist (hoist (hoist identityToIO)) (runGameComputation comp)

-- | Lift DisplayM to GameT - add Reader and ExceptT layers
liftDisplay :: (Monad  m) => DisplayT m a -> GameT m a
liftDisplay display = GameT $ lift $ lift (runDisplayT display)
