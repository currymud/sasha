module Build.BedPuzzle.BedRoom where
import           Build.Identifiers.Actions                            (agentCanSeeGID)
import           Data.Map.Strict                                      (Map,
                                                                       fromList)
import qualified Data.Map.Strict
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ActionF,
                                                                       Location (Location, _locationActionManagement),
                                                                       _objectLabelMap,
                                                                       _title)
import           Model.GID                                            (GID)
import           Model.Mappings                                       (LabelToGIDListMapping (LabelToGIDListMapping))
import           Model.Parser.GCase                                   (VerbKey (ImplicitStimulusKey))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectLabelMap = LabelToGIDListMapping Data.Map.Strict.empty
  , _locationActionManagement = actionMap
  }

actionMap :: Map VerbKey (GID ActionF)
actionMap = fromList [(ImplicitStimulusKey look, agentCanSeeGID)]
