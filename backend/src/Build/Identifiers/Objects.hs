module Build.Identifiers.Objects  where
import           Build.BedPuzzle.Actions.Objects.Chair (chairObj)
import           Build.BedPuzzle.Actions.Objects.Pill  (pillObj)
import           Build.BedPuzzle.Actions.Objects.Table (tableObj)
import           Build.Templates.Identification        (makeObjectGIDsAndMap)

makeObjectGIDsAndMap [[| pillObj|], [| tableObj |], [| chairObj |]]
