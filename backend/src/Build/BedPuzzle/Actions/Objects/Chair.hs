module Build.BedPuzzle.Actions.Objects.Chair where
import           Build.Identifiers.Actions                               (whatChairGID)
import           Data.Map.Strict                                         (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (small,
                                                                          white)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (pill,
                                                                          table)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          ActionManagement (DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          ConsumptionActionF,
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName),
                                                                          SomaticAccessActionF,
                                                                          SpatialRelationship (Contains),
                                                                          World (_spatialRelationshipMap))
import           Model.GID                                               (GID)
import           Model.Parser.Atomics.Verbs                              (AcquisitionVerb (AcquisitionVerb),
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb,
                                                                          SomaticAccessVerb)
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase,
                                                                          ConsumptionVerbPhrase)

chairObj :: Object
chairObj =
  let
  longDescription = DirectionalStimulusNounPhrase (DescriptiveNounPhraseDet the small table)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase table)
  in Object
       { _shortName = "a chair"
       , _description = "It's the chair next to your bed."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = directionalStimulusVerbs
    directionalStimulusVerbs :: ActionManagementFunctions
    directionalStimulusVerbs = ActionManagementFunctions $ Data.Set.singleton (DSAManagementKey look whatChairGID)
