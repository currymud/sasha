module Build.BedPuzzle.Actions.Objects.Robe where
import           Build.Identifiers.Actions                               (getRobeGID,
                                                                          whatRobeGID)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (table)
import           Grammar.Parser.Partitions.Nouns.Objectives              (robe)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionManagement (ActionManagement),
                                                                          ConsumptionActionF,
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                                          SomaticAccessActionF)
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Nouns                              (Objective)
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
  longDescription = DirectionalStimulusNounPhrase (NounPhrase the table)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase table)
  in Object
       { _shortName = "a robe"
       , _description = "your bathrobe"
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagement
    verbMaps = ActionManagement directionalStimulusVerbs implicitStimulusVerbs somaticAccessVerbs acquisitionVerbs consumptionVerbs
    implicitStimulusVerbs :: Map ImplicitStimulusVerb (GID ImplicitStimulusActionF)
    implicitStimulusVerbs = Data.Map.Strict.empty
    directionalStimulusVerbs :: Map DirectionalStimulusVerb (GID DirectionalStimulusActionF)
    directionalStimulusVerbs =  Data.Map.Strict.fromList [(look, whatRobeGID :: GID DirectionalStimulusActionF)]
    somaticAccessVerbs :: Map SomaticAccessVerb (GID SomaticAccessActionF)
    somaticAccessVerbs = Data.Map.Strict.empty
    acquisitionVerbs :: Map AcquisitionVerbPhrase (GID AcquisitionActionF)
    acquisitionVerbs = Data.Map.Strict.fromList [(getRobeAVP, getRobeGID)]
    consumptionVerbs :: Map ConsumptionVerbPhrase (GID ConsumptionActionF)
    consumptionVerbs = Data.Map.Strict.empty


-- Add helper definitions
getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get robeObjective

robeObjective :: ObjectPhrase
robeObjective = ObjectPhrase robeNP

robeNP :: NounPhrase Objective
robeNP = SimpleNounPhrase robe
