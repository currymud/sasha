module Build.Identifiers.Objects  where

import           Build.BedPuzzle.Objects        (playerOBJ)
import           Build.Templates.Identification (makeObjectGIDsAndMap)

makeObjectGIDsAndMap [[| playerOBJ|]]
