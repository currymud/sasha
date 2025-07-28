module Build.Identifiers.Objects  where
import           Build.BedPuzzle.Actions.Objects.Pill (pillObj)
import           Build.Templates.Identification       (makeObjectGIDsAndMap)

-- objectMap :: GIDToDataMap Object
makeObjectGIDsAndMap [[| pillObj|]]
