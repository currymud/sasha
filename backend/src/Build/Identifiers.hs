module Build.Identifiers where
import           Actions.Percieve.Look          (agentCanSee, agentCannotSee)
import           Build.Templates.Identification (makeActionGIDsAndMap,
                                                 makeLabels)
import           Data.Text                      (Text)
import           Grammar.Parser.Lexer           (Lexeme (BED))
import           Model.GameState                (Location)
import           Model.Label                    (Label (Label))

acs = agentCannotSee pitchBlack
pitchBlack :: Text
pitchBlack = "It is pitch black"

makeActionGIDsAndMap [[| agentCanSee |], [| acs |]]
makeLabels ''Location [("bedRoomInBed", BED)]

