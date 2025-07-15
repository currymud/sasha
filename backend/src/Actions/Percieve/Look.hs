module Actions.Percieve.Look (agentCanSee,agentCannotSee) where
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text)
import           Model.GameState        (ActionF (ImplicitStimulusF),
                                         ResolutionT (ResolutionT))


agentCanSee :: ActionF (ResolutionT ())
agentCanSee = ImplicitStimulusF $ Right $ \msg ->
  ResolutionT  $ liftIO $ print msg

agentCannotSee :: Text -> ActionF (ResolutionT ())
agentCannotSee nosee = ImplicitStimulusF $ Left $ ResolutionT $
  liftIO $ print nosee
