{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.GameState (
  ActionF (ComputeLocation, ComputeAction, ExecuteAction)
  , ActionMap
  , Evaluator
  , GameComputation
  , GameStateExceptT (GameStateExceptT)
  , runGameStateExceptT
  , GameState (GameState, _world, _player, _narration, _evaluation)
  , Location (Location, _title, _objectLabelMap, _locationActionManagement)
  , Narration (Narration, _playerAction, _actionConsequence)
  , Object ( Object, _shortName, _description, _descriptives
           , _objectActionManagement)
  , Config (Config, _actionMap)
  , Player (Player, _location, _object)
  , PlayerEvaluator
  , ProcessSentence (ImplicitStimulusF)
  , ResolutionT (ResolutionT,runResolutionT)
  , World (World, _objectMap,_locationMap)) where

import           Control.Monad.Except (ExceptT, MonadError)
import           Control.Monad.Reader (MonadReader, ReaderT)
import           Control.Monad.State  (MonadIO, MonadState, StateT)
import           Data.Kind            (Type)
import           Data.Map.Strict      (Map)
import           Data.Set             (Set)
import           Data.Text            (Text)
import           Model.GID            (GID)
import           Model.Label          (Label)
import           Model.Mappings       (GIDToDataMap, LabelToGIDListMapping)
import           Model.Parser         (Sentence)
import           Model.Parser.GCase   (VerbKey)

type GameComputation :: Type
type GameComputation = ResolutionT (GameStateExceptT ())

type ActionF :: Type
data ActionF
  = ComputeImplicitStimulusAction (GID Location -> GameComputation)

data ProcessSentence
  = ImplicitStimulusF (Either (ResolutionT ()) (Text -> ResolutionT ()))

type Evaluator :: Type
type Evaluator
  = (Sentence -> GameStateExceptT (Location -> ResolutionT ()))

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

-- The ActionMap and other unchangeables
type Config :: Type
data Config = Config
  { _actionMap :: ActionMap
  }

type Location :: Type
data Location = Location {
    _title                    :: Text
  , _objectLabelMap           :: LabelToGIDListMapping Object Object
  , _locationActionManagement :: Map VerbKey (GID ActionF)
}

type Player :: Type
data Player = Player
  { _location :: GID Location
  , _object   :: GID Object
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
