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
import qualified Grammar.Parser.Partitions.Nouns.DirectionalStimulus          (chair,
                                                                               pill,
                                                                               table)
import qualified Grammar.Parser.Partitions.Nouns.Objectives                   (chair,
                                                                               table)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb      (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitRegionalStimulusVerb (wait)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb         (look)
import           Model.GameState                                              (AcquisitionActionF,
                                                                               ActionManagement (ActionManagement),
                                                                               DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                                               ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                               Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                                                               Object,
                                                                               SomaticAccessActionF)
import           Model.GID                                                    (GID)
import           Model.Parser.Atomics.Nouns                                   (DirectionalStimulus,
                                                                               Objective)
import           Model.Parser.Atomics.Verbs                                   (AcquisitionVerb,
                                                                               DirectionalStimulusVerb,
                                                                               ImplicitStimulusVerb,
                                                                               SomaticAccessVerb)
import           Model.Parser.GCase                                           (NounKey (DirectionalStimulusKey, ObjectiveKey),
                                                                               VerbKey (ImplicitStimulusKey))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectSemanticMap = objectSemanticMap
  , _locationActionManagement = actionMap
  }

dirChair :: DirectionalStimulus
dirChair = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.chair

dirTable :: DirectionalStimulus
dirTable = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.table

dirPill :: DirectionalStimulus
dirPill = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.pill

objChair :: Objective
objChair = Grammar.Parser.Partitions.Nouns.Objectives.chair

objTable :: Objective
objTable = Grammar.Parser.Partitions.Nouns.Objectives.table

objectSemanticMap :: Map NounKey (GID Object)
objectSemanticMap = Data.Map.Strict.fromList [ (DirectionalStimulusKey dirChair, chairObjGID)
                                             , (DirectionalStimulusKey dirTable, tableObjGID)
                                             , (DirectionalStimulusKey dirPill, pillObjGID)
                                             , (ObjectiveKey objChair, chairObjGID)
                                             , (ObjectiveKey objTable, tableObjGID)
                                             ]

actionMap :: ActionManagement
actionMap = ActionManagement directionalStimulus implicitStimulus somaticAccessVerbs acquisitionVerbs
 where
   implicitStimulus :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
   implicitStimulus = Data.Map.Strict.fromList [(implicitStimulusLook, pitchBlackFGID)]
   directionalStimulus :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
   directionalStimulus = Data.Map.Strict.fromList [(directionalStimulusLook, lookAtGID)]
   somaticAccessVerbs :: Map SomaticAccessVerb (GID SomaticAccessActionF)
   somaticAccessVerbs = Data.Map.Strict.empty
   acquisitionVerbs :: Map AcquisitionVerb (GID AcquisitionActionF)
   acquisitionVerbs = Data.Map.Strict.empty
implicitStimulusLook :: ImplicitStimulusVerb
implicitStimulusLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

directionalStimulusLook :: DirectionalStimulusVerb
directionalStimulusLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
