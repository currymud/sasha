module GameState (getActionF, liftGS) where
import           Control.Monad.Reader (MonadReader (ask))
import qualified Data.Map.Strict
import           Error                (throwMaybeM)
import           Model.GameState      (ActionF, Config (_actionMap),
                                       GameStateExceptT,
                                       ResolutionT (ResolutionT))
import           Model.GID            (GID)
import           Model.Mappings       (_getGIDToDataMap)

getActionF :: GID ActionF -> GameStateExceptT ActionF
getActionF vkey = do
  gs :: Config <- ask
  let amap = _getGIDToDataMap $ _actionMap gs
  throwMaybeM "Action not found in action map" $ Data.Map.Strict.lookup vkey amap


liftGS :: GameStateExceptT a -> ResolutionT a
liftGS = ResolutionT

