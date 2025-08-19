module Location where
import           Control.Monad.Identity     (Identity)
import           Control.Monad.State.Strict (MonadState (get), modify')
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            (lookup)
import           Data.Text                  (Text, pack)
import           Error                      (throwMaybeM)
import           Model.GameState            (ActionManagementFunctions,
                                             GameComputation,
                                             Location (_locationActionManagement),
                                             _location, _locationMap, _player,
                                             _world)
import           Model.GameState.Mappings   (_getGIDToDataMap)
import           Model.GID                  (GID)

getLocationIdM :: GameComputation Identity (GID Location)
getLocationIdM = get <&> (_location . _player)

updateLocationIdM :: GID Location -> GameComputation Identity ()
updateLocationIdM newLocID = do
  player <- get <&> _player
  modify' (\gs -> gs {_player = player {_location = newLocID}})

getLocationActionMapM :: GameComputation Identity ActionManagementFunctions
getLocationActionMapM = _locationActionManagement
                          <$> getPlayerLocationM

getLocationM :: GID Location -> GameComputation Identity Location
getLocationM locID = do
  locationMap <- get <&> (_getGIDToDataMap . _locationMap . _world)
  throwMaybeM err_msg $  Data.Map.Strict.lookup locID locationMap
  where
    err_msg :: Text
    err_msg = pack
      $ "Location with ID " ++ show locID ++ " not found in the location map."

getPlayerLocationM :: GameComputation Identity Location
getPlayerLocationM =  getLocationIdM >>= getLocationM
