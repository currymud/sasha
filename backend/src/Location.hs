module Location where
import           Control.Monad.State.Strict (MonadState (get), modify')
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            (lookup)
import           Data.Text                  (Text, pack)
import           Error                      (throwMaybeM)
import           Model.GameState            (GameStateExceptT, Location,
                                             _location, _locationMap, _player,
                                             _world)
import           Model.GID                  (GID)
import           Model.Mappings             (_getGIDToDataMap)
getLocationID :: GameStateExceptT (GID Location)
getLocationID = get <&> (_location . _player)

updateLocationID :: GID Location -> GameStateExceptT ()
updateLocationID newLocID = do
  player <- get <&> _player
  modify' (\gs -> gs {_player = player {_location = newLocID}})

getLocation :: GID Location -> GameStateExceptT Location
getLocation locID = do
  locationMap <- get <&> (_getGIDToDataMap . _locationMap . _world)
  throwMaybeM err_msg $  Data.Map.Strict.lookup locID locationMap
  where
    err_msg :: Text
    err_msg = pack
      $ "Location with ID " ++ show locID ++ " not found in the location map."
