module Build.BedPuzzle.Actions.Objects.Mail where
import           Build.Identifiers.Actions                               (getMailDeniedFGID,
                                                                          notEvenMailGID)
import qualified Data.Set
import           Grammar.Parser.Partitions.Misc                          (the)
import           Grammar.Parser.Partitions.Nouns.DirectionalStimulus     (mail)
import           Grammar.Parser.Partitions.Nouns.Objectives              (mail)
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

mailObj :: Object
mailObj =
  let
  longDescription = DirectionalStimulusNounPhrase (NounPhrase the dsMailDirectional)
  shortDescription = DirectionalStimulusNounPhrase (SimpleNounPhrase dsMailDirectional)
  in Object
       { _shortName = "junk mail"
       , _description = "A pile of junk mail scattered on the table."
       , _descriptives = Data.Set.fromList [longDescription,shortDescription]
       , _objectActionManagement = verbMaps
       }
  where
    verbMaps :: ActionManagementFunctions
    verbMaps = ActionManagementFunctions $ Data.Set.fromList
      [ DSAManagementKey look notEvenMailGID
      ]

mailObjective :: Objective
mailObjective = Grammar.Parser.Partitions.Nouns.Objectives.mail

dsMailDirectional :: DirectionalStimulus
dsMailDirectional = Grammar.Parser.Partitions.Nouns.DirectionalStimulus.mail

mailObjectPhrase :: ObjectPhrase
mailObjectPhrase = ObjectPhrase mailNP

getMailAVP :: AcquisitionVerbPhrase
getMailAVP = SimpleAcquisitionVerbPhrase get mailObjectPhrase

mailNP :: NounPhrase Objective
mailNP = SimpleNounPhrase mailObjective
