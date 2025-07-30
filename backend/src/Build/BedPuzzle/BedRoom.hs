module Build.BedPuzzle.BedRoom where
import           Build.Identifiers.Actions                                    (agentCanSeeGID)
import           Build.Identifiers.Objects                                    (pillObjGID)
import           Data.Map.Strict                                              (Map,
                                                                               empty,
                                                                               fromList)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus          (pill)
import           Grammar.Parser.Partitions.Verbs.ImplicitRegionalStimulusVerb (wait)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb         (look)
import           Model.GameState                                              (ActionManagement (ActionManagement),
                                                                               DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                                               ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                               Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                                                               Object)
import           Model.GID                                                    (GID)
import           Model.Parser.Atomics.Verbs                                   (DirectionalStimulusVerb,
                                                                               ImplicitStimulusVerb)
import           Model.Parser.GCase                                           (NounKey (DirectionalStimulusKey),
                                                                               VerbKey (ImplicitStimulusKey))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectSemanticMap = objectSemanticMap
  , _locationActionManagement = actionMap
  }

objectSemanticMap :: Map NounKey (GID Object)
objectSemanticMap = Data.Map.Strict.fromList [(DirectionalStimulusKey pill,pillObjGID)]

actionMap :: ActionManagement
actionMap = ActionManagement directionalStimulus implicitStimulus
 where
   implicitStimulus :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
   implicitStimulus = Data.Map.Strict.fromList [(implicitStimulusLook, agentCanSeeGID)]
   directionalStimulus :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
   directionalStimulus = Data.Map.Strict.empty

implicitStimulusLook :: ImplicitStimulusVerb
implicitStimulusLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

