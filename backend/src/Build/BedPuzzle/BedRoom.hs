module Build.BedPuzzle.BedRoom where
import           Build.Identifiers.Actions                            (agentCanSeeGID)
import           Build.Identifiers.Objects                            (pillObjGID)
import           Data.Map.Strict                                      (Map,
                                                                       fromList)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus  (pill)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ActionF,
                                                                       Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                                                       Object)
import           Model.GID                                            (GID)
import           Model.Parser.GCase                                   (NounKey (DirectionalStimulusKey),
                                                                       VerbKey (ImplicitStimulusKey))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectSemanticMap = objectSemanticMap
  , _locationActionManagement = actionMap
  }

objectSemanticMap :: Map NounKey (GID Object)
objectSemanticMap = Data.Map.Strict.fromList [(DirectionalStimulusKey pill,pillObjGID)]

actionMap :: Map VerbKey (GID ActionF)
actionMap = Data.Map.Strict.fromList [(implicitStimulusLook, agentCanSeeGID)]

implicitStimulusLook :: VerbKey
implicitStimulusLook = ImplicitStimulusKey Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

