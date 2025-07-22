{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.GameState (
  ActionF (ImplicitStimulusAction)
  , ActionMap
  , Config (Config, _actionMap, _sentenceProcessingMaps)
  , DisplayM (DisplayM, runDisplayM)
  , Evaluator
  , GameComputation (GameComputation, runGameComputation)
  , GameStateExceptT (GameStateExceptT)
  , runGameStateExceptT
  , GameState (GameState, _world, _player, _narration, _evaluation)
  , GameT (GameT, runGameT)
  , Location (Location, _title, _objectLabelMap, _locationActionManagement)
  , Narration (Narration, _playerAction, _actionConsequence)
  , Object ( Object, _shortName, _description, _descriptives
           , _objectActionManagement)
  , Player (Player, _location, _sentenceManagement)
  , PlayerEvaluator
  , ProcessImplicitVerbMap
  , PlayerProcessImplicitVerbMap
  , PlayerSentenceProcessingMaps (PlayerSentenceProcessingMaps, _playerProcessImplicitVerbMap)
  , ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb, _unProcessImplicitStimlusVerb)
  , ProcessImplicitVerbMaps
  , ResolutionT (ResolutionT,runResolutionT)
  , SentenceProcessingMaps (SentenceProcessingMaps, _processImplicitVerbMap)
  , transformToIO, liftDisplay
  , World (World, _objectMap,_locationMap)
  , updateActionConsequence, updatePlayerAction) where

import           Control.Monad.Except       (ExceptT, MonadError)
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.Morph        (MFunctor (hoist))
import           Control.Monad.Reader       (MonadReader, ReaderT)
import           Control.Monad.State        (MonadIO, MonadState, StateT)
import           Control.Monad.Trans        (MonadTrans (lift))
import           Data.Kind                  (Type)
import           Data.Map.Strict            (Map)
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           Model.GID                  (GID)
import           Model.Label                (Label)
import           Model.Mappings             (GIDToDataMap,
                                             LabelToGIDListMapping)
import           Model.Parser               (Sentence)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Model.Parser.GCase         (VerbKey)


-- | Display monad - no config, no errors, just state + IO
type DisplayM :: (Type -> Type) -> Type -> Type
newtype DisplayM m a = DisplayM
  { runDisplayM :: StateT GameState m a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadState GameState, MonadIO
    )

instance MonadTrans DisplayM where
  lift = DisplayM . lift

type GameComputation :: (Type -> Type) -> Type -> Type
newtype GameComputation m a = GameComputation
  { runGameComputation :: ReaderT Config (ExceptT Text (StateT GameState m)) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Config, MonadError Text, MonadState GameState
    )
instance MonadTrans GameComputation where
  lift = GameComputation . lift . lift . lift

type GameT :: (Type -> Type) -> Type -> Type
newtype GameT m a = GameT
  { runGameT :: ReaderT Config (ExceptT Text (StateT GameState m)) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Config, MonadError Text, MonadState GameState, MonadIO
    )
instance MonadTrans GameT where
  lift = GameT . lift . lift . lift

type ActionF :: Type
data ActionF
  = ImplicitStimulusAction (Location -> GameComputation Identity ())

-- The ActionMap and other unchangeables
type Config :: Type
data Config = Config
  { _actionMap              :: ActionMap
  , _sentenceProcessingMaps :: SentenceProcessingMaps
  }

type SentenceProcessingMaps :: Type
data SentenceProcessingMaps = SentenceProcessingMaps
  {_processImplicitVerbMap :: ProcessImplicitVerbMaps}

type ProcessImplicitVerbMap :: Type
type ProcessImplicitVerbMap = Map (GID ProcessImplicitStimulusVerb) ProcessImplicitStimulusVerb

type ProcessImplicitVerbMaps :: Type
type ProcessImplicitVerbMaps = Map ImplicitStimulusVerb ProcessImplicitVerbMap

type PlayerProcessImplicitVerbMap :: Type
type PlayerProcessImplicitVerbMap = Map ImplicitStimulusVerb (GID ProcessImplicitStimulusVerb)

type PlayerSentenceProcessingMaps :: Type
data PlayerSentenceProcessingMaps = PlayerSentenceProcessingMaps
  { _playerProcessImplicitVerbMap :: PlayerProcessImplicitVerbMap
  }

type ProcessImplicitStimulusVerb :: Type
newtype ProcessImplicitStimulusVerb = ProcessImplicitStimulusVerb { _unProcessImplicitStimlusVerb :: ImplicitStimulusVerb -> GameComputation Identity () }

type Evaluator :: Type
type Evaluator
  = (Sentence -> GameComputation Identity ())

type PlayerEvaluator :: Type
type PlayerEvaluator
  = (VerbKey -> GameStateExceptT (ResolutionT Location))

type GameStateExceptT :: Type -> Type
newtype GameStateExceptT a = GameStateExceptT
  { runGameStateExceptT :: ReaderT Config (ExceptT Text (StateT GameState IO)) a
  }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadReader Config
                   , MonadError Text
                   , MonadState GameState
                   , MonadIO)

type ActionMap :: Type
type ActionMap = GIDToDataMap ActionF ActionF

type GameState :: Type
data GameState = GameState
  { _world      :: World
  , _player     :: Player
  , _narration  :: Narration
  , _evaluation :: Evaluator
  }

type Narration :: Type
data Narration = Narration
  { _playerAction      :: [Text] -- what player tried to do
  , _actionConsequence :: [Text] -- what happened as a result of the action
  }
  deriving stock (Show)

updatePlayerAction :: Text -> Narration -> Narration
updatePlayerAction action narration =
  narration { _playerAction = action : _playerAction narration }

updateActionConsequence :: Text -> Narration -> Narration
updateActionConsequence consequence narration =
  narration { _actionConsequence = consequence : _actionConsequence narration }

type Location :: Type
data Location = Location {
    _title                    :: Text
  , _objectLabelMap           :: LabelToGIDListMapping Object Object
  , _locationActionManagement :: Map VerbKey (GID ActionF)
}

type Player :: Type
data Player = Player
  { _location           :: GID Location
  , _sentenceManagement :: PlayerProcessImplicitVerbMap
  }

type Object :: Type
data Object = Object
 { _shortName              :: Text
 , _description            :: Text
 , _descriptives           :: Set (Label Text)
 , _objectActionManagement :: Map VerbKey (GID ActionF) -- Placeholder for action management logic
 }

type ResolutionT :: Type -> Type
newtype ResolutionT a = ResolutionT
  { runResolutionT :: GameStateExceptT a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadReader Config
                   , MonadError Text
                   , MonadState GameState
                   , MonadIO)
type World :: Type
data World = World
  { _objectMap   :: GIDToDataMap Object Object
  , _locationMap :: GIDToDataMap Location Location
  }


identityToIO :: Identity a -> IO a
identityToIO = return . runIdentity

-- | Transform GameComputation Identity to GameT IO - change base monad using hoist
-- This is much cleaner than manual unwrapping!
transformToIO :: GameComputation Identity a -> GameT IO a
transformToIO comp = GameT $ hoist (hoist (hoist identityToIO)) (runGameComputation comp)

-- | Lift DisplayM to GameT - add Reader and ExceptT layers
liftDisplay :: (Monad  m) => DisplayM m a -> GameT m a
liftDisplay display = GameT $ lift $ lift (runDisplayM display)
