module GameState where
import           Control.Monad.State (get)
import qualified Data.Map.Strict
import           Error               (throwMaybeM)
import           Model.GameState     (ActionF, ActionMap,
                                      GameState (_actionMap), GameStateExceptT,
                                      ResolutionT, unActionMap)
import           Model.GID           (GID)
import           Model.Mappings      (_getGIDToDataMap)

getActionF :: GID (ActionF (ResolutionT ())) -> GameStateExceptT (ActionF (ResolutionT ()))
getActionF vkey = do
  gs :: GameState <- get
  let amap = _getGIDToDataMap $ unActionMap $ _actionMap gs
  throwMaybeM "Action not found in action map" $ Data.Map.Strict.lookup vkey amap

