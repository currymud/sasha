{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.Actions where
import           Actions.Percieve.Look                                   (agentCanSee,
                                                                          dsvActionEnabled,
                                                                          isvActionEnabled,
                                                                          lookAt)
import qualified Build.BedPuzzle.Actions.Get
import           Build.BedPuzzle.Actions.Inventory                       (checkInventory)
import qualified Build.BedPuzzle.Actions.Locations.Bedroom.Get
import           Build.BedPuzzle.Actions.Look                            (pitchBlackF)
import           Build.BedPuzzle.Actions.Objects.Chair.Look              (seeChair,
                                                                          whatChair)
import           Build.BedPuzzle.Actions.Objects.Pill.Get                (getPill,
                                                                          getPillDenied)
import           Build.BedPuzzle.Actions.Objects.Pill.Look               (notEvenPill,
                                                                          seePill,
                                                                          whatPill)
import           Build.BedPuzzle.Actions.Objects.Pocket.Look             (emptyPocket,
                                                                          notEvenPocket,
                                                                          seePocketChair,
                                                                          seePocketRobeWorn,
                                                                          whatPocket)
import           Build.BedPuzzle.Actions.Objects.Robe.Get                (alreadyHaveRobe,
                                                                          getRobe,
                                                                          getRobeDenied)
import           Build.BedPuzzle.Actions.Objects.Robe.Look               (notEvenRobe,
                                                                          seeRobeChair,
                                                                          whatRobe)
import           Build.BedPuzzle.Actions.Objects.Table.Look              (seeTable,
                                                                          whatTable)
import           Build.BedPuzzle.Actions.Open                            (openEyes,
                                                                          openEyesDenied)
import           Build.Templates.Identification                          (makeAcquisitionActionGIDsAndMap,
                                                                          makeDirectionalStimulusActionGIDsAndMap,
                                                                          makeImplicitStimulusActionGIDsAndMap,
                                                                          makeSomaticAccessActionGIDsAndMap)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF)

isaEnabledLook :: ImplicitStimulusActionF
isaEnabledLook =
  isvActionEnabled Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dsvEnabledLook :: DirectionalStimulusActionF
dsvEnabledLook =
  dsvActionEnabled Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

locGet :: AcquisitionActionF
locGet = Build.BedPuzzle.Actions.Locations.Bedroom.Get.get
-- implicitStimulusActionMap

playerGet :: AcquisitionActionF
playerGet = Build.BedPuzzle.Actions.Get.get

dizzyGet :: AcquisitionActionF
dizzyGet = Build.BedPuzzle.Actions.Get.getDenied

getDenied :: AcquisitionActionF
getDenied = Build.BedPuzzle.Actions.Locations.Bedroom.Get.getDenied

makeImplicitStimulusActionGIDsAndMap [[| agentCanSee |],
                                      [| pitchBlackF |],
                                      [| isaEnabledLook |],
                                      [| checkInventory |]]

-- directionalStimulusActionMap
makeDirectionalStimulusActionGIDsAndMap [[| seePill |]
                                          , [| notEvenPill|]
                                          , [| whatPill |]
                                          , [| dsvEnabledLook|]
                                          , [| lookAt |]
                                          , [| whatTable |]
                                          , [| seeTable |]
                                          , [| whatChair |]
                                          , [| seeChair|]
                                          , [| whatRobe|]
                                          , [| notEvenRobe|]
                                          , [| whatPocket |]
                                          , [| notEvenPocket |]
                                          , [| seePocketChair |]
                                          , [| seePocketRobeWorn |]
                                          , [| emptyPocket |]
                                          , [| seeRobeChair |]]

makeSomaticAccessActionGIDsAndMap [[|openEyesDenied |], [| openEyes|]]

makeAcquisitionActionGIDsAndMap [[| locGet |],
                                 [| getDenied |],
                                 [| dizzyGet |],
                                 [| playerGet|],
                                 [| getPillDenied |],
                                 [| getPill|],
                                 [| alreadyHaveRobe |],
                                 [| getRobeDenied |],
                                 [| getRobe|]]

