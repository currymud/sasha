module Build.BedPuzzle.BedRoom where
import qualified Data.Map.Strict
import           Model.GameState (Location (Location, _locationActionManagement),
                                  _objectLabelMap, _title)
import           Model.Mappings  (LabelToGIDListMapping (LabelToGIDListMapping))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectLabelMap = LabelToGIDListMapping Data.Map.Strict.empty
  , _locationActionManagement = []
  }
