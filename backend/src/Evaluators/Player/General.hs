module Evaluators.Player.General where
import           Model.GameState    (GameStateExceptT, Location)
import           Model.Parser.GCase (VerbKey)

-- does the player have this verbkey in their map? If yes, return Location if not return
evalPlayer :: VerbKey -> GameStateExceptT Location
evalPlayer = undefined
