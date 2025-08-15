module Build.BedPuzzle.BedRoom where
import           Build.BedPuzzle.Actions.Objects.Mail                         (getMailAVP)
import           Build.Identifiers.Actions                                    (agentCanSeeGID,
                                                                               lookAtGID,
                                                                               pitchBlackFGID)
import           Build.Identifiers.Objects                                    (chairObjGID,
                                                                               mailObjGID,
                                                                               pillObjGID,
                                                                               robeObjGID,
                                                                               tableObjGID)
import           Data.Map.Strict                                              (Map,
                                                                               empty,
                                                                               fromList)
import           Data.Set                                                     (Set)
import qualified Data.Set
import qualified Grammar.Parser.Partitions.Nouns.Consumables                  (pill)
import qualified Grammar.Parser.Partitions.Nouns.DirectionalStimulus          (chair,
                                                                               mail,
                                                                               pill,
                                                                               robe,
                                                                               table)
import           Grammar.Parser.Partitions.Nouns.Objectives                   (pill,
                                                                               robe)
import qualified Grammar.Parser.Partitions.Nouns.Objectives                   (chair,
                                                                               mail,
                                                                               pill,
                                                                               robe,
                                                                               table)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs             (acquisitionVerbs,
                                                                               get)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb      (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitRegionalStimulusVerb (wait)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb         (look)
import           Model.GameState                                              (AcquisitionActionF,
                                                                               ActionManagement (AAManagementKey, DSAManagementKey, ISAManagementKey),
                                                                               ActionManagementFunctions (ActionManagementFunctions),
                                                                               ConsumptionActionF,
                                                                               DirectionalStimulusActionF (DirectionalStimulusActionF),
                                                                               ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                                               Location (Location, _locationActionManagement, _objectSemanticMap, _title),
                                                                               Object,
                                                                               SomaticAccessActionF)
import           Model.GID                                                    (GID)
import           Model.Parser.Atomics.Nouns                                   (Consumable,
                                                                               DirectionalStimulus,
                                                                               Objective)
import           Model.Parser.Atomics.Verbs                                   (AcquisitionVerb,
                                                                               DirectionalStimulusVerb,
                                                                               ImplicitStimulusVerb,
                                                                               SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                                (NounPhrase (SimpleNounPhrase),
                                                                               ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                                (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                               ConsumptionVerbPhrase)
import           Model.Parser.GCase                                           (NounKey (ConsumableNounKey, DirectionalStimulusKey, ObjectiveKey),
                                                                               VerbKey (ImplicitStimulusKey))


bedroomInBed :: Location
bedroomInBed = Location
  { _title = "Bedroom in Bed"
  , _objectSemanticMap = objectSemanticMap
  , _locationActionManagement = actionMap
  }


dirMail :: DirectionalStimulus
dirMail = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.mail

dirChair :: DirectionalStimulus
dirChair = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.chair

dirTable :: DirectionalStimulus
dirTable = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.table

dirPill :: DirectionalStimulus
dirPill = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.pill

dirRobe :: DirectionalStimulus
dirRobe = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.robe

objChair :: Objective
objChair = Grammar.Parser.Partitions.Nouns.Objectives.chair

objTable :: Objective
objTable = Grammar.Parser.Partitions.Nouns.Objectives.table

objPill :: Objective
objPill = Grammar.Parser.Partitions.Nouns.Objectives.pill

objRobe :: Objective
objRobe = Grammar.Parser.Partitions.Nouns.Objectives.robe

objMail :: Objective
objMail = Grammar.Parser.Partitions.Nouns.Objectives.mail

comPill :: Consumable
comPill = Grammar.Parser.Partitions.Nouns.Consumables.pill

objectSemanticMap :: Map NounKey (Set (GID Object))
objectSemanticMap = Data.Map.Strict.fromList sList
  where
    sList =
      [(ConsumableNounKey comPill, Data.Set.singleton pillObjGID)
      , (DirectionalStimulusKey dirChair, Data.Set.singleton chairObjGID)
      , (DirectionalStimulusKey dirTable, Data.Set.singleton tableObjGID)
      , (DirectionalStimulusKey dirRobe, Data.Set.singleton robeObjGID)
      , (DirectionalStimulusKey dirPill, Data.Set.singleton pillObjGID)
      , (DirectionalStimulusKey dirMail, Data.Set.singleton mailObjGID)
      , (ObjectiveKey objChair, Data.Set.singleton chairObjGID)
      , (ObjectiveKey objTable, Data.Set.singleton tableObjGID)
      , (ObjectiveKey objRobe, Data.Set.singleton robeObjGID)
      , (ObjectiveKey objPill, Data.Set.singleton pillObjGID)
      , (ObjectiveKey objMail, Data.Set.singleton mailObjGID)
      ]

actionMap :: ActionManagementFunctions
actionMap = ActionManagementFunctions $ Data.Set.fromList [directionalStimulus, implicitStimulus] <> acquisitionVerbs'
  where
   implicitStimulus :: ActionManagement
   implicitStimulus = ISAManagementKey implicitStimulusLook pitchBlackFGID
   directionalStimulus :: ActionManagement
   directionalStimulus = DSAManagementKey directionalStimulusLook lookAtGID
   acquisitionVerbs' =
-- this needs adjustment
     Data.Set.fromList [ -- AAManagementKey getRobeAVP locGetGID
                            --             , AAManagementKey getPillAVP getPillDeniedGID
                            --             , AAManagementKey getMailAVP getMailDeniedGID
                                         ]

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get robeObjective

getPillAVP :: AcquisitionVerbPhrase
getPillAVP = SimpleAcquisitionVerbPhrase get pillObjective

pillObjective :: ObjectPhrase
pillObjective = ObjectPhrase pillNP

pillNP :: NounPhrase Objective
pillNP = SimpleNounPhrase pill

robeObjective :: ObjectPhrase
robeObjective = ObjectPhrase robeNP

robeNP :: NounPhrase Objective
robeNP = SimpleNounPhrase robe
implicitStimulusLook :: ImplicitStimulusVerb
implicitStimulusLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

directionalStimulusLook :: DirectionalStimulusVerb
directionalStimulusLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
