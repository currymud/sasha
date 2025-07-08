module Build.Identifiers where
import           Actions.Percieve.Look          (agentCanSee)
import           Build.Templates.Identification (makeActionGIDsAuto, makeLabels)
import           Grammar.Parser.Lexer           (Lexeme (BED))
import           Model.GameState                (Location)
import           Model.Label                    (Label (Label))
$(makeActionGIDsAuto
  [ [| agentCanSee |]
  ])

makeLabels ''Location [("bedRoomInBed", BED)]
