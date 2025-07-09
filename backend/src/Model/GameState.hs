{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Model.GameState (
  ActionF (ImplicitStimulusF)
  , Evaluator (Evaluator)
  , GameStateExceptT (GameStateExceptT)
  , runGameStateExceptT
  , GameState (..)
  , Location (Location, _title, _objectLabelMap, _locationActionManagement)
  , Object ( Object, _shortName, _entityLabel, _description, _descriptives
           , _objectActionManagement)
  , Config
  , Player (Player, _location, _object)
  , ResolutionF (ResolutionF,runResolutionF)
  , World (World, _objectMap,_locationMap)) where

import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader   (MonadReader, ReaderT)
import           Control.Monad.State    (MonadIO, MonadState, StateT)
import           Data.Kind              (Type)
import           Data.Map.Strict        (Map)
import           Data.Set               (Set)
import           Data.Text              (Text)
import           Model.GID              (GID)
import           Model.Label            (Label)
import           Model.Mappings         (GIDToDataMap, LabelToGIDListMapping)
import           Model.Parser.GCase     (VerbKeys)

type ActionF :: Type -> Type
data ActionF a
  = ImplicitStimulusF (Either a (GID Location -> a))
  {-

Evaluator should not return a GameStateExceptT(), but rather return a ResolutionT , which holds GameStateExceptT function)


      -}
type Evaluator :: Type
newtype Evaluator = Evaluator (Identity ())-- Evaluator { runEvaluator :: Imperative -> GameStateExceptT ()}

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

type GameState :: Type
data GameState = GameState
  { _world      :: World
  , _player     :: Player
  , _narration  :: Narration
  , _evaluation :: Evaluator
  , _actionMap  :: GIDToDataMap (ActionF ResolutionF) (ActionF ResolutionF)
  }

type Narration :: Type
data Narration = Narration
  { _playerAction      :: [Text]
  , _actionConsequence :: GID Text
  }
  deriving stock (Show)

type Config :: Type
data Config = Config

type Location :: Type
data Location = Location {
    _title                    :: Text
  , _objectLabelMap           :: LabelToGIDListMapping Object Object
  , _locationActionManagement :: Map VerbKeys (GID (ActionF ResolutionF))
}

type Player :: Type
data Player = Player
  { _location :: GID Location
  , _object   :: GID Object
  }

type Object :: Type
data Object = Object
 { _shortName              :: Text
 , _entityLabel            :: Label Object
 , _description            :: Text
 , _descriptives           :: Set (Label Text)
 , _objectActionManagement :: Identity () -- Placeholder for action management logic
 }

type ResolutionF :: Type
newtype ResolutionF = ResolutionF
  { runResolutionF :: GameStateExceptT () }
  {-
     I want the action management system to ultimately produce a ResolutionT, which will be a monad transformer that can be used to run the GameStateExceptT

     the evaluator will manage the computation, but will be informed by the computation in the _actionManagement field

     The action management system needs a way to describe the relationship between objects and verbs.

for example having parsed "plant the pot plant in the plant pot", the plant object would need a way to describe that it's the
object being planted, the pot object would be the object being acted upon, and the trowel object
would be the instrument doing thhe planting. The structure of Sentence needs to be leveraged
  -}

type World :: Type
data World = World
  { _objectMap   :: GIDToDataMap Object Object
  , _locationMap :: GIDToDataMap Location Location
  }
