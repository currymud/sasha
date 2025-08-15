module Build.BedPuzzle.Actions.Objects.Table where
import qualified Data.Set
import           Grammar.Parser.Partitions.Adjectives                    (small)
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (table)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName))
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (DescriptiveNounPhraseDet, SimpleNounPhrase))

tableObj :: Object
tableObj =
  let
  longDescription = DirectionalStimulusNounPhrase (DescriptiveNounPhraseDet the small table)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase table)
  in Object
       { _shortName = "table"
       , _description = "It's the table next to your bed."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = directionalStimulusVerbs

    directionalStimulusVerbs :: ActionManagementFunctions
    directionalStimulusVerbs =  ActionManagementFunctions $ Data.Set.empty -- (DSAManagementKey look whatTableGID)
