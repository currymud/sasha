module Actions.Percieve.Look where
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text)
import           Location               (getLocation)
import           Model.GameState        (Location (_title),
                                         ResolutionF (ResolutionF))
import           Model.GID              (GID)


agentCanSee :: Either ResolutionF (GID Location -> ResolutionF)
agentCanSee = Right $ \locationGID ->
  ResolutionF $ do
    desc <- _title <$> getLocation locationGID
    liftIO $ print desc

agentCannotSee :: Text -> Either ResolutionF (GID Location -> ResolutionF)
agentCannotSee nosee = Left $ ResolutionF $
  liftIO $ print nosee
