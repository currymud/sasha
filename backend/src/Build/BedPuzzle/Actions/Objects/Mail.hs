module Build.BedPuzzle.Actions.Objects.Mail where
import           Build.Identifiers.Actions                               (notEvenMailGID)
import qualified Data.Set
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (mail)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Model.GameState                                         (ActionManagement (DSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          Object (Object, _description, _descriptives, _objectActionManagement, _shortName))
import           Model.Parser.Composites.Nouns                           (DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                                          NounPhrase (NounPhrase, SimpleNounPhrase))

mailObj :: Object
mailObj =
  let
  longDescription = DirectionalStimulusNounPhrase (NounPhrase the mail)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase mail)
  in Object
       { _shortName = "junk mail"
       , _description = "A pile of junk mail scattered on the table."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = directionalStimulusVerbs

    directionalStimulusVerbs :: ActionManagementFunctions
    directionalStimulusVerbs = ActionManagementFunctions
                                 $ Data.Set.singleton (DSAManagementKey look notEvenMailGID)
