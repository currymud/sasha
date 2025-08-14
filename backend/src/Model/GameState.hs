{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.GameState (
  ActionEffect (SomaticAccessActionEffect,ImplicitStimulusActionEffect)
  , ActionEffectKey (LocationKey, ObjectKey, PlayerKey)
  , ActionEffectMap (ActionEffectMap, _actionEffectMap)
  , ActionKey ( AcquisitionalActionKey,
                ConsumptionActionKey,
                ImplicitStimulusActionKey,
                DirectionalStimulusActionKey,
                PosturalActionKey,
                SomaticAccessActionKey)
  , ActionKeyMap (ActionKeyMap, _unActionKeyMap)
  , ActionManagement (DSAManagementKey, ISAManagementKey, SSAManagementKey, AAManagementKey, CAManagementKey,
                     PPManagementKey, NPManagementKey)
  , ActionManagementFunctions (ActionManagementFunctions, _actionManagementFunctions)
  , ActionMaps (ActionMaps
                 , _acquisitionActionMap
                 , _implicitStimulusActionMap
                 , _directionalStimulusActionMap
                 , _posturalActionMap
                 ,_somaticStimulusActionMap
                 ,_acquisitionActionMap
                 , _consumptionActionMap)
  , AcquisitionActionF (AcquisitionActionF,CollectedF,LosesObjectF)
  , ConsumptionActionF (ConsumptionActionF, _consumptionAction)
  , ConsumptionActionMap
  , Config (Config, _actionMaps)
  , DirectionalStimulusActionF (DirectionalStimulusActionF, _directionalStimulusAction)
  , DirectionalStimulusActionMap
  , DisplayT (DisplayT, runDisplayT)
  , Effect ( AcquisitionEffect
               , ConsumptionEffect
               , ImplicitStimulusEffect
               , DirectionalStimulusEffect
               , PerceptionEffect
               , PositivePosturalEffect
               , NegativePosturalEffect
               , SomaticAccessEffect)
  , EffectRegistry
  , Evaluator
  , GameComputation (GameComputation, runGameComputation)
  , GameState (GameState, _world, _player, _narration, _evaluation, _effectRegistry)
  , GameStateT (GameStateT, runGameStateT)
  , GameT (GameT, runGameT)
  , ImplicitStimulusActionF (ImplicitStimulusActionF, _implicitStimulusAction)
  , ImplicitStimulusActionMap
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
  , SomaticAccessActionF (SomaticAccessActionF, _somaticAccessAction)
  , SomaticStimulusActionMap
  , SpatialRelationship (ContainedIn, Contains, Inventory, Supports, SupportedBy)
  , SpatialRelationshipMap (SpatialRelationshipMap, _spatialRelationshipMap)
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
import           Model.GID                     (GID)
import           Model.Mappings                (GIDToDataMap)
import           Model.Parser                  (Sentence)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                ConsumptionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase,
                                                ObjectPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ConsumptionVerbPhrase)
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
newtype DirectionalStimulusActionF = DirectionalStimulusActionF
  { _directionalStimulusAction :: DirectionalStimulusNounPhrase -> GID Object -> GameComputation Identity () }

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
  { _somaticAccessAction :: Set ActionEffectKey -> ActionEffectMap -> GameComputation Identity () }

type PosturalActionF :: Type
newtype PosturalActionF = PosturalActionF
  { _positivePosturalAction :: Set ActionEffectKey -> ActionEffectMap -> GameComputation Identity () }

type SearchStrategy :: Type
type SearchStrategy = NounKey
                        -> GameComputation Identity (Maybe (GID Object, GID Object))

type AcquisitionActionF :: Type
data AcquisitionActionF
 = AcquisitionActionF (SearchStrategy -> AcquisitionVerbPhrase -> GameComputation Identity ())
 | CollectedF (GID Object -> Either (GameComputation Identity ()) (GameComputation Identity ()))
 | LosesObjectF (GID Object -> GID Object -> Either (GameComputation Identity ()) (GameComputation Identity ()))

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

type PlayerKey :: Type
data PlayerKey
  = PlayerKeyLocation (GID Location)
  | PlayerKeyObject (GID Object)
  deriving stock (Show, Eq, Ord)

type ActionKey :: Type
data ActionKey
  = ImplicitStimulusActionKey (GID ImplicitStimulusActionF)
  | DirectionalStimulusActionKey (GID DirectionalStimulusActionF)
  | SomaticAccessActionKey (GID SomaticAccessActionF)
  | AcquisitionalActionKey (GID AcquisitionActionF)
  | ConsumptionActionKey (GID ConsumptionActionF)
  | PosturalActionKey (GID PosturalActionF)
  deriving stock (Show, Eq, Ord)

type Effect :: Type
data Effect
  = ImplicitStimulusEffect ImplicitStimulusVerb (GID ImplicitStimulusActionF)
  | DirectionalStimulusEffect DirectionalStimulusVerb (GID DirectionalStimulusActionF)
  | SomaticAccessEffect SomaticAccessVerb (GID SomaticAccessActionF)
  | AcquisitionEffect AcquisitionVerbPhrase (GID AcquisitionActionF)
  | ConsumptionEffect ConsumptionVerb (GID Object) (GID ConsumptionActionF)
  | PositivePosturalEffect PositivePosturalVerb (GID PosturalActionF)
  | NegativePosturalEffect NegativePosturalVerb (GID PosturalActionF)
  | PerceptionEffect
  deriving stock (Show, Eq, Ord)

type EffectRegistry = Map ActionKey ActionEffectMap

type ActionEffect :: Type
data ActionEffect
  = SomaticAccessActionEffect (Map (GID SomaticAccessActionF) Effect)
  | AcquisitionActionEffect (Map (GID AcquisitionActionF) Effect)
  | ImplicitStimulusActionEffect (Map (GID ImplicitStimulusActionF) (Map ActionEffectKey Effect))
  | EdibleConsumptionActionEffect (Map (GID ConsumptionActionF) Effect)
  deriving stock (Show, Eq, Ord)

type ActionEffectMap :: Type
newtype ActionEffectMap = ActionEffectMap
  { _actionEffectMap :: Map ActionEffectKey (Set Effect)}
  deriving stock (Show, Eq, Ord)

type ActionKeyMap :: Type
newtype ActionKeyMap = ActionKeyMap
  { _unActionKeyMap :: Map ActionKey ActionEffectMap }
  deriving stock (Show, Eq, Ord)

type Config :: Type
data Config = Config
  { _actionMaps             :: ActionMaps
  }

type GameState :: Type
data GameState = GameState
  { _world          :: World
  , _player         :: Player
  , _narration      :: Narration
  , _evaluation     :: Evaluator
  , _effectRegistry :: EffectRegistry
  }

type Location :: Type
data Location = Location {
    _title                    :: Text
  , _objectSemanticMap        :: Map NounKey (Set (GID Object))
  , _locationActionManagement :: ActionManagementFunctions
--  , _locationEffects          :: LocationEffects
}

type ActionManagement :: Type
data ActionManagement
  = DSAManagementKey DirectionalStimulusVerb (GID DirectionalStimulusActionF)
  | ISAManagementKey ImplicitStimulusVerb (GID ImplicitStimulusActionF)
  | SSAManagementKey SomaticAccessVerb (GID SomaticAccessActionF)
  | AAManagementKey AcquisitionVerbPhrase (GID AcquisitionActionF)
  | CAManagementKey ConsumptionVerbPhrase (GID ConsumptionActionF)
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
