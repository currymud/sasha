module Actions.Percieve.Look (agentCanSee,agentCannotSee) where
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text)
import           Location               (getLocation)
import           Model.GameState        (ActionF (ImplicitStimulusF),
                                         Location (_title),
                                         ResolutionF (ResolutionF))


agentCanSee :: ActionF ResolutionF
agentCanSee = ImplicitStimulusF $ Right $ \loc ->
  ResolutionF $ liftIO $ print (_title loc)

agentCannotSee :: Text -> ActionF ResolutionF
agentCannotSee nosee = ImplicitStimulusF $ Left $ ResolutionF $
  liftIO $ print nosee
