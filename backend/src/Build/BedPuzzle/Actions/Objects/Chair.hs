module Build.BedPuzzle.Actions.Objects.Chair where
import           Build.Identifiers.Actions                               (whatChairFGID)
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (chair)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName))
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

chairObj :: Object
chairObj =
  let
  longDescription = DirectionalStimulusNounPhrase (DescriptiveNounPhraseDet the small chair)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase chair)
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
    directionalStimulusVerbs = ActionManagementFunctions $ Data.Set.singleton (DSAManagementKey look whatChairFGID)
