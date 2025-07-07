module Actions.Percieve.Look where
import           Location        (getLocation)
import           Model.GameState (Location (_title), ResolutionF (ResolutionF))
import           Model.GID       (GID)

agentEyesOpen :: Either ResolutionF (GID Location -> ResolutionF)
agentEyesOpen = Right $ \locationGID -> do
     desc <- _title <$> getLocation locationGID
     ResolutionF $ pure $ print desc

agentEyesClosed :: Either ResolutionF (GID Location -> ResolutionF)
agentEyesClosed = undefined
