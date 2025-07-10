module Build.BedPuzzle.Player where
import           Build.Identifiers.Locations (bedroomInBedGID)
import           Build.Identifiers.Objects   (playerOBJGID)
import           Model.GameState             (Player (Player))

player :: Player
player = Player bedroomInBedGID playerOBJGID

