module Build.Identifiers.Objects  where
import           Build.BedPuzzle.Actions.Objects.Chair  (chairObj)
import           Build.BedPuzzle.Actions.Objects.Mail   (mailObj)
import           Build.BedPuzzle.Actions.Objects.Pill   (pillObj)
import           Build.BedPuzzle.Actions.Objects.Pocket (pocketObj)
import           Build.BedPuzzle.Actions.Objects.Robe   (robeObj)
import           Build.BedPuzzle.Actions.Objects.Table  (tableObj)
import           Build.Templates.Identification         (makeObjectGIDsAndMap)

makeObjectGIDsAndMap [ [| mailObj |]
                     , [| pillObj|]
                     , [| tableObj |]
                     , [| chairObj |]
                     , [| robeObj |]
                     , [| pocketObj |]]
