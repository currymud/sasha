module Build.BedPuzzle.Actions.Objects.Floor where
import qualified Data.Set
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (floor)
import           Grammar.Parser.Partitions.Nouns.Objectives              (floor,
                                                                          mail,
                                                                          pill,
                                                                          robe)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (AAManagementKey, DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName))
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus,
                                                                          Objective)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (NounPhrase, SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase))

floorObj :: Object
floorObj =
  let
  longDescription = DirectionalStimulusNounPhrase (NounPhrase the dsFloor)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase dsFloor)
  in Object
       { _shortName = "floor"
       , _description = "The bedroom floor - wooden planks that have seen better days."
       , _descriptives = Data.Set.fromList [longDescription, shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = ActionManagementFunctions $ Data.Set.fromList
      [--  DSAManagementKey look seeFloorGID
    --  , AAManagementKey getFloorAVP getFloorGID
      -- Add other objects that can be on the floor
      ]

-- Helper definitions
dsFloor :: DirectionalStimulus
dsFloor = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.floor

floorObjective :: Objective
floorObjective = Grammar.Parser.Partitions.Nouns.Objectives.floor

getFloorAVP :: AcquisitionVerbPhrase
getFloorAVP = SimpleAcquisitionVerbPhrase get floorObjectPhrase

floorObjectPhrase :: ObjectPhrase
floorObjectPhrase = ObjectPhrase floorNP

floorNP :: NounPhrase Objective
floorNP = SimpleNounPhrase floorObjective

-- Acquisition verb phrases for floor removal actions
getRobeFromFloorAVP :: AcquisitionVerbPhrase
getRobeFromFloorAVP = SimpleAcquisitionVerbPhrase get robeObjectPhrase

getMailFromFloorAVP :: AcquisitionVerbPhrase
getMailFromFloorAVP = SimpleAcquisitionVerbPhrase get mailObjectPhrase

getPillFromFloorAVP :: AcquisitionVerbPhrase
getPillFromFloorAVP = SimpleAcquisitionVerbPhrase get pillObjectPhrase
robeObjectPhrase :: ObjectPhrase
robeObjectPhrase = ObjectPhrase (SimpleNounPhrase Grammar.Parser.Partitions.Nouns.Objectives.robe)

mailObjectPhrase :: ObjectPhrase
mailObjectPhrase = ObjectPhrase (SimpleNounPhrase Grammar.Parser.Partitions.Nouns.Objectives.mail)

pillObjectPhrase :: ObjectPhrase
pillObjectPhrase = ObjectPhrase (SimpleNounPhrase Grammar.Parser.Partitions.Nouns.Objectives.pill)
