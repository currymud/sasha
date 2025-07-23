module Build.BedPuzzle.Actions.Objects.Pill where
import qualified Data.Map.Strict
import qualified Data.Set
import           Model.GameState (Object (Object, _description, _descriptives, _objectActionManagement, _shortName))

pill :: Object
pill = Object
  { _shortName = "pill"
  , _description = "A small, round pill. Probably good for headaches."
  , _descriptives = Data.Set.empty
  , _objectActionManagement = Data.Map.Strict.empty
  }
