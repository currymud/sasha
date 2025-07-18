module Actions.Percieve.Look (agentCanSee,agentCannotSee, manageImplicitStimulusProcess) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Text                  (Text)
import           GameState                  (getPlayerActionF)
import           Location                   (getLocationM, getPlayerLocationM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             GameComputation, GameStateExceptT,
                                             Location (_title),
                                             ProcessSentence (ImplicitStimulusF),
                                             ResolutionT (ResolutionT))

import           Control.Monad.RWS          (MonadReader (local))
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)

agentCanSee :: ActionF
agentCanSee = ImplicitStimulusAction (\loc ->
  pure $ ResolutionT $ liftIO $ print ("You see: " <> _title loc))

agentCannotSee :: Text -> ActionF
agentCannotSee nosee = ImplicitStimulusAction $ \loc ->
  pure $ ResolutionT $ liftIO $ print nosee

manageImplicitStimulusProcess :: ProcessSentence
manageImplicitStimulusProcess = ImplicitStimulusF (\isv ->
  pure $ ResolutionT $ pure ())

noSeeLoc :: Text -> GameComputation
noSeeLoc nosee = pure $ ResolutionT $ liftIO $ print nosee
