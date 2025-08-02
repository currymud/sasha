module Build.BedPuzzle.BedRoom where
import           Build.Identifiers.Actions                                    (agentCanSeeGID,
                                                                               lookAtGID,
                                                                               pitchBlackFGID)
import           Build.Identifiers.Objects                                    (chairObjGID,
                                                                               pillObjGID,
                                                                               tableObjGID)
import           Data.Map.Strict                                              (Map,
                                                                               empty,
                                                                               fromList)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus          (chair,
                                                                               pill,
                                                                               table)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb      (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitRegionalStimulusVerb (wait)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb         (look)
import           Model.GameState                                              (ActionManagement (ActionManagement),
                                                                               DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                                               ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                               Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                                                               Object,
                                                                               SomaticAccessActionF)
import           Model.GID                                                    (GID)
import           Model.Parser.Atomics.Verbs                                   (DirectionalStimulusVerb,
                                                                               ImplicitStimulusVerb,
                                                                               SomaticAccessVerb)
import           Model.Parser.GCase                                           (NounKey (DirectionalStimulusKey),
                                                                               VerbKey (ImplicitStimulusKey))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectSemanticMap = objectSemanticMap
  , _locationActionManagement = actionMap
  }

objectSemanticMap :: Map NounKey (GID Object)
objectSemanticMap = Data.Map.Strict.fromList [ (DirectionalStimulusKey chair, chairObjGID)
                                             , (DirectionalStimulusKey table, tableObjGID)]

actionMap :: ActionManagement
actionMap = ActionManagement directionalStimulus implicitStimulus somaticAccessVerbs
 where
   implicitStimulus :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
   implicitStimulus = Data.Map.Strict.fromList [(implicitStimulusLook, pitchBlackFGID)]
   directionalStimulus :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
   directionalStimulus = Data.Map.Strict.fromList [(directionalStimulusLook, lookAtGID)]
   somaticAccessVerbs :: Map SomaticAccessVerb (GID SomaticAccessActionF)
   somaticAccessVerbs = Data.Map.Strict.empty

implicitStimulusLook :: ImplicitStimulusVerb
implicitStimulusLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

directionalStimulusLook :: DirectionalStimulusVerb
directionalStimulusLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
