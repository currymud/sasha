module Actions.Percieve.Look (agentCanSee,agentCannotSee) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text)
import           Model.GameState        (ActionF (ComputeAction, ComputeLocation, ExecuteAction),
                                         Location (_title),
                                         ProcessSentence (ImplicitStimulusF),
                                         ResolutionT (ResolutionT))

import           Control.Monad.RWS      (MonadReader (local))
import           Location               (getLocationM)

-- has the ability to see and will compute a Location
agentCanSee :: ActionF
agentCanSee = ComputeLocation (ResolutionT . getLocationM)

generalSeen :: Location -> ActionF
generalSeen loc = ExecuteAction $ ResolutionT $
  pure $ liftIO $ print $ "You see: " <> _title loc
agentCannotSee :: Text -> ActionF
agentCannotSee nosee = ComputeAction $ ImplicitStimulusF $ Left $ ResolutionT $
  liftIO $ print nosee
