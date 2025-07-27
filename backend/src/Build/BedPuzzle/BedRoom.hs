module Build.BedPuzzle.BedRoom where
import           Build.Identifiers.Actions                            (agentCanSeeGID)
import           Data.Map.Strict                                      (Map,
                                                                       fromList)
import qualified Data.Map.Strict
import           Data.Set                                             (Set)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ActionF,
                                                                       Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                                                       Object)
import           Model.GID                                            (GID)
import           Model.Parser.GCase                                   (NounKey,
                                                                       VerbKey (ImplicitStimulusKey))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectSemanticMap = objectSemanticMap
  , _locationActionManagement = actionMap
  }

objectSemanticMap :: Map NounKey (Set (GID Object))
objectSemanticMap = Data.Map.Strict.empty

actionMap :: Map VerbKey (GID ActionF)
actionMap = fromList [(ImplicitStimulusKey look, agentCanSeeGID)]
