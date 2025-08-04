module Build.Inventory (initialInventory) where
import           Model.GameState (ActionManagement (ActionManagement),
                                  Object (Object, _description, _descriptives, _objectActionManagement, _shortName))

initialInventory :: Object
initialInventory = Object
  { _shortName = mempty
  , _description = mempty
  , _descriptives = mempty
  , _objectActionManagement = ActionManagement mempty mempty mempty mempty
  }

