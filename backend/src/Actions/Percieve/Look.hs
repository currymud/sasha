module Actions.Percieve.Look (agentCanSee,agentCannotSee, manageImplicitStimulusProcess) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text)
import           Model.GameState        (ActionF (ImplicitStimulusAction),
                                         GameComputation, Location (_title),
                                         ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb),
                                         ResolutionT (ResolutionT))


agentCanSee :: ActionF
agentCanSee = ImplicitStimulusAction (\loc ->
  pure $ ResolutionT $ liftIO $ print ("You see: " <> _title loc))

agentCannotSee :: Text -> ActionF
agentCannotSee nosee = ImplicitStimulusAction $ \loc ->
  pure $ ResolutionT $ liftIO $ print nosee

manageImplicitStimulusProcess :: ProcessImplicitStimulusVerb
manageImplicitStimulusProcess = ProcessImplicitStimulusVerb (\isv ->
  pure $ ResolutionT $ pure ())

noSeeLoc :: Text -> GameComputation
noSeeLoc nosee = pure $ ResolutionT $ liftIO $ print nosee
