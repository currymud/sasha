module Build.BedPuzzle.Actions.Objects.Robe where
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Misc                          (the)
import qualified Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (robe)
import           Grammar.Parser.Partitions.Nouns.Objectives              (robe)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionManagement (AAManagementKey, DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          ConsumptionActionF,
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Nouns                              (DirectionalStimulus,
                                                                          Objective)
import           Model.Parser.Atomics.Verbs                              (DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (NounPhrase, SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase)

robeObj :: Object
robeObj =
  let
  longDescription = DirectionalStimulusNounPhrase (NounPhrase the dsRobe)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase dsRobe)
  in Object
       { _shortName = "a robe"
       , _description = "your bathrobe"
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = ActionManagementFunctions $ Data.Set.fromList
      [ -- DSAManagementKey look whatRobeGID
      ]
dsRobe :: DirectionalStimulus
dsRobe = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.robe
-- Add helper definitions
getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get robeObjective

robeObjective :: ObjectPhrase
robeObjective = ObjectPhrase robeNP

robeNP :: NounPhrase Objective
robeNP = SimpleNounPhrase robe
