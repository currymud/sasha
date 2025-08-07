module Build.BedPuzzle.BedRoom where
import           Build.Identifiers.Actions                                    (agentCanSeeGID,
                                                                               getRobeGID,
                                                                               lookAtGID,
                                                                               pitchBlackFGID)
import           Build.Identifiers.Objects                                    (chairObjGID,
                                                                               pillObjGID,
                                                                               pocketObjGID,
                                                                               robeObjGID,
                                                                               tableObjGID)
import           Data.Map.Strict                                              (Map,
                                                                               empty,
                                                                               fromList)
import           Data.Set                                                     (Set)
import qualified Data.Set
import qualified Grammar.Parser.Partitions.Nouns.DirectionalStimulus          (chair,
                                                                               pill,
                                                                               pocket,
                                                                               robe,
                                                                               table)
import           Grammar.Parser.Partitions.Nouns.Objectives                   (robe)
import qualified Grammar.Parser.Partitions.Nouns.Objectives                   (chair,
                                                                               pill,
                                                                               pocket,
                                                                               robe,
                                                                               table)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs             (get)
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
import           Model.Parser.Composites.Nouns                                (NounPhrase (SimpleNounPhrase),
                                                                               ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                                (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase))
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

dirRobe :: DirectionalStimulus
dirRobe = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.robe

dirPocket :: DirectionalStimulus
dirPocket = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.pocket

objChair :: Objective
objChair = Grammar.Parser.Partitions.Nouns.Objectives.chair

objTable :: Objective
objTable = Grammar.Parser.Partitions.Nouns.Objectives.table

objPill :: Objective
objPill = Grammar.Parser.Partitions.Nouns.Objectives.pill

objPocket :: Objective
objPocket = Grammar.Parser.Partitions.Nouns.Objectives.pocket

objRobe :: Objective
objRobe = Grammar.Parser.Partitions.Nouns.Objectives.robe

objectSemanticMap :: Map NounKey (Set (GID Object))
objectSemanticMap = Data.Map.Strict.fromList sList
  where
    sList =
      [ (DirectionalStimulusKey dirChair, Data.Set.singleton chairObjGID)
      , (DirectionalStimulusKey dirTable, Data.Set.singleton tableObjGID)
      , (ObjectiveKey objChair, Data.Set.singleton chairObjGID)
      , (ObjectiveKey objTable, Data.Set.singleton tableObjGID)
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
   acquisitionVerbs :: Map AcquisitionVerbPhrase (GID AcquisitionActionF)
   acquisitionVerbs = Data.Map.Strict.fromList [(getRobeAVP, getRobeGID)]

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get robeObjective

robeObjective :: ObjectPhrase
robeObjective = ObjectPhrase robeNP

robeNP :: NounPhrase Objective
robeNP = SimpleNounPhrase robe

implicitStimulusLook :: ImplicitStimulusVerb
implicitStimulusLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

directionalStimulusLook :: DirectionalStimulusVerb
directionalStimulusLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
