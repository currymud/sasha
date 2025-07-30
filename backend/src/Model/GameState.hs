{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.GameState (
  ActionManagement (ActionManagement, _directionalStimulusActionManagement, _implicitStimulusActionManagement)
  , ActionMaps (ActionMaps, _implicitStimulusActionMap, _directionalStimulusActionMap)
  , Config (Config, _actionMaps)
  , DirectionalStimulusActionF (DirectionalStimulusActionF, _directionalStimulusAction)
  , DirectionalStimulusActionMap
  , DisplayT (DisplayT, runDisplayT)
  , Evaluator
  , GameComputation (GameComputation, runGameComputation)
  , GameState (GameState, _world, _player, _narration, _evaluation)
  , GameStateT (GameStateT, runGameStateT)
  , GameT (GameT, runGameT)
  , ImplicitStimulusActionF (ImplicitStimulusActionF, _implicitStimulusAction)
  , ImplicitStimulusActionMap
  , Location (Location, _title, _objectSemanticMap, _locationActionManagement)
  , Narration (Narration, _playerAction, _actionConsequence)
  , Object (Object, _shortName, _description, _descriptives, _objectActionManagement)
  , PlayerSentenceProcessingMaps (PlayerSentenceProcessingMaps, _playerProcessImplicitVerbMap)
  , SentenceProcessingMaps (SentenceProcessingMaps, _processImplicitVerbMap)
  , transformToIO, liftDisplay
  , fromDisplay
  , Perceptables (Perceptables, _perceptables)
  , Player (Player, _location, _perceptables, _playerActions)
  , PlayerActions (PlayerActions, _implicitStimulusActions,_directionalStimulusActions)
  , PlayerProcessImplicitVerbMap
  , ProcessDirectionalStimulusVerb (ProcessDirectionalStimulusVerb, _unProcessDirectionalStimlusVerb)
  , ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb, _unProcessImplicitStimlusVerb)
  , ProcessImplicitVerbMap
  , ProcessImplicitVerbMaps
  , SpatialRelationship (ContainedIn, Contains, Supports, SupportedBy)
  , SpatialRelationshipMap (SpatialRelationshipMap, _spatialRelationshipMap)
  , World (World, _objectMap, _locationMap)
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
import           Model.Parser.Atomics.Nouns    (DirectionalStimulus)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase,
                                                NounPhrase)
import           Model.Parser.GCase            (NounKey, VerbKey)

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

  {-
-- Computation builders
type ActionF :: Type
data ActionF
  = ImplicitStimulusAction (Location -> GameComputation Identity ())
  | DirectionalStimulusAction (GameComputation Identity ())
-}

type ActionMaps :: Type
data ActionMaps = ActionMaps
  { _implicitStimulusActionMap    :: ImplicitStimulusActionMap
  , _directionalStimulusActionMap :: DirectionalStimulusActionMap
  }

type ImplicitStimulusActionMap :: Type
type ImplicitStimulusActionMap = Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF

type ImplicitStimulusActionF :: Type
newtype ImplicitStimulusActionF = ImplicitStimulusActionF
  { _implicitStimulusAction :: Location -> GameComputation Identity () }

type DirectionalStimulusActionMap :: Type
type DirectionalStimulusActionMap = Map (GID DirectionalStimulusActionF) DirectionalStimulusActionF

type DirectionalStimulusActionF :: Type
newtype DirectionalStimulusActionF = DirectionalStimulusActionF
  { _directionalStimulusAction :: DirectionalStimulusNounPhrase -> Location -> GID Object -> GameComputation Identity () }

-- Sentence Processing Maps

type SentenceProcessingMaps :: Type
data SentenceProcessingMaps = SentenceProcessingMaps
  { _processImplicitVerbMap    :: ProcessImplicitVerbMaps
--  , _processDirectionalVerbMap :: ProcessDirectionalStimulusVerbMaps
  }

type ProcessImplicitVerbMap :: Type
type ProcessImplicitVerbMap = Map (GID ProcessImplicitStimulusVerb) (ImplicitStimulusVerb -> ImplicitStimulusActionF)

type ProcessDirectionalVerbMap :: Type
type ProcessDirectionalVerbMap = Map (GID ProcessDirectionalStimulusVerb) ProcessDirectionalStimulusVerb

type ProcessImplicitVerbMaps :: Type
type ProcessImplicitVerbMaps = Map ImplicitStimulusVerb ProcessImplicitVerbMap

type ProcessDirectionalStimulusVerbMaps :: Type
type ProcessDirectionalStimulusVerbMaps = Map DirectionalStimulusVerb ProcessDirectionalVerbMap

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

type PlayerSentenceProcessingMaps :: Type
data PlayerSentenceProcessingMaps = PlayerSentenceProcessingMaps
  { _playerProcessImplicitVerbMap    :: PlayerProcessImplicitVerbMap
  }

type Evaluator :: Type
type Evaluator
  = (Sentence -> GameComputation Identity ())

-- Game Objects

type Config :: Type
data Config = Config
  { _actionMaps             :: ActionMaps
--  , _sentenceProcessingMaps :: SentenceProcessingMaps
  }

type GameState :: Type
data GameState = GameState
  { _world      :: World
  , _player     :: Player
  , _narration  :: Narration
  , _evaluation :: Evaluator
  }

type Location :: Type
data Location = Location {
    _title                    :: Text
  , _objectSemanticMap        :: Map NounKey (GID Object)
  , _locationActionManagement :: ActionManagement
}

type ActionManagement :: Type
data ActionManagement = ActionManagement
  { _directionalStimulusActionManagement :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
  , _implicitStimulusActionManagement :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
  }
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
  , _playerActions :: PlayerActions
  , _perceptables  :: Perceptables
  }

type PlayerActions :: Type
data PlayerActions = PlayerActions
 { _implicitStimulusActions :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
 , _directionalStimulusActions :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
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
  deriving stock (Show, Eq, Ord)

type Perceptables :: Type
newtype Perceptables = Perceptables
  { _perceptables :: Set (GID Object) }
  deriving stock (Show,Eq,Ord)

type World :: Type
data World = World
  { _objectMap              :: GIDToDataMap Object Object
  , _locationMap            :: GIDToDataMap Location Location
  , _perceptionMap          :: Map DirectionalStimulus (Set (GID Object))
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
 , _objectActionManagement :: ActionManagement
 }

-- | Lift GameStateM to GameComputation

-- | Lift GameStateM to DisplayM (this is just id, but for clarity)
liftToDisplay :: GameStateT m a -> DisplayT m a
liftToDisplay = DisplayT

-- | Convert DisplayM to GameStateM (for when you need the base layer)
fromDisplay :: DisplayT m a -> GameStateT m a
fromDisplay = runDisplayT

identityToIO :: Identity a -> IO a
identityToIO = return . runIdentity

-- | Transform GameComputation Identity to GameT IO - change base monad using hoist
-- This is much cleaner than manual unwrapping!
transformToIO :: GameComputation Identity a -> GameT IO a
transformToIO comp = GameT $ hoist (hoist (hoist identityToIO)) (runGameComputation comp)

-- | Lift DisplayM to GameT - add Reader and ExceptT layers
liftDisplay :: (Monad  m) => DisplayT m a -> GameT m a
liftDisplay display = GameT $ lift $ lift (runDisplayT display)
