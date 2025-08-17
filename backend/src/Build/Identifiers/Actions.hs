{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.Actions where
import           Actions.Percieve.Look                                   (agentCanSee,
                                                                          dsvActionEnabled,
                                                                          isvActionEnabled,
                                                                          lookAt)
import qualified Build.BedPuzzle.Actions.Get
import           Build.BedPuzzle.Actions.Get.Constructors                (getFromSupportF,
                                                                          getObjectF)

import           Build.BedPuzzle.Actions.Get                             (getDeniedF)
import           Build.BedPuzzle.Actions.Inventory                       (checkInventory)
import           Build.BedPuzzle.Actions.Look                            (pitchBlackF)
import           Build.BedPuzzle.Actions.Objects.Chair.Look              (seeChairF,
                                                                          whatChairF)
import           Build.BedPuzzle.Actions.Objects.Floor.Get               (getFloorDeniedF)
import           Build.BedPuzzle.Actions.Objects.Floor.Look              (notEvenFloorF,
                                                                          seeFloorF)
import           Build.BedPuzzle.Actions.Objects.Mail.Get                (alreadyHaveMailF,
                                                                          getMailDeniedF,
                                                                          getMailDizzyF)
import           Build.BedPuzzle.Actions.Objects.Mail.Look               (notEvenMail,
                                                                          seeMail,
                                                                          whatMail)
import           Build.BedPuzzle.Actions.Objects.Pill.Get                (getPillDeniedF)
import           Build.BedPuzzle.Actions.Objects.Pill.Look               (notEvenPill,
                                                                          seePill,
                                                                          whatPill)
import           Build.BedPuzzle.Actions.Objects.Pill.Take               (alreadyTookPillF,
                                                                          pillTooFarF,
                                                                          takePillDeniedF,
                                                                          takePillF)
import           Build.BedPuzzle.Actions.Objects.Pocket.Look             (emptyPocket,
                                                                          notEvenPocket,
                                                                          seePocketChair,
                                                                          seePocketRobeWorn,
                                                                          whatPocket)
import           Build.BedPuzzle.Actions.Objects.Robe.Get                (alreadyHaveRobeF,
                                                                          getRobeDeniedF)
import           Build.BedPuzzle.Actions.Objects.Robe.Look               (notEvenRobe,
                                                                          seeRobeChair,
                                                                          seeRobeWorn,
                                                                          whatRobe)
import           Build.BedPuzzle.Actions.Objects.Table.Look              (seeTable,
                                                                          whatTable)
import           Build.BedPuzzle.Actions.Open                            (openEyes,
                                                                          openEyesDenied)
import           Build.BedPuzzle.Actions.Stand                           (standDenied,
                                                                          standUp)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS         (chairGID,
                                                                          robeGID)
import           Build.Templates.Identification                          (makeAcquisitionActionGIDsAndMap,
                                                                          makeConsumptionActionGIDsAndMap,
                                                                          makeDirectionalStimulusActionGIDsAndMap,
                                                                          makeImplicitStimulusActionGIDsAndMap,
                                                                          makePosturalActionGIDsAndMap,
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

playerGetF :: AcquisitionActionF
playerGetF = Build.BedPuzzle.Actions.Get.getF

dizzyGetF :: AcquisitionActionF
dizzyGetF = Build.BedPuzzle.Actions.Get.getDeniedF

getRobeF :: AcquisitionActionF
getRobeF = getObjectF robeGID

robeCollectedF :: AcquisitionActionF
robeCollectedF = getObjectF robeGID

getFromChairF :: AcquisitionActionF
getFromChairF = getFromSupportF chairGID

getFromRobeF :: AcquisitionActionF
getFromRobeF = getFromSupportF robeGID

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
                                          , [| whatChairF |]
                                          , [| seeChairF|]
                                          , [| whatRobe|]
                                          , [| notEvenRobe|]
                                          , [| whatPocket |]
                                          , [| notEvenPocket |]
                                          , [| seePocketChair |]
                                          , [| seePocketRobeWorn |]
                                          , [| emptyPocket |]
                                          , [| seeRobeChair |]
                                          , [| seeRobeWorn  |]
                                          , [| seeMail |]
                                          , [| notEvenMail |]
                                          , [| whatMail |]
                                          , [| notEvenFloorF |]
                                          , [| seeFloorF|]]

makeSomaticAccessActionGIDsAndMap [[|openEyesDenied |], [| openEyes|]]

makeAcquisitionActionGIDsAndMap [ [| alreadyHaveMailF|],

                                  [| getMailDeniedF |],
                                  [|  alreadyHaveRobeF |],
                                  [| playerGetF|],
                                  [| dizzyGetF |],
                                  [| getRobeF |],
                                  [| getFromChairF |],
                                  [| robeCollectedF|], [| getFromRobeF|]]
                                    {-
                                 [| getDenied |],
                                 [| getPillDeniedF |],
                                 [| alreadyHaveRobeF |],
                                 [| getRobeDeniedF |],
                                [| getFloorDeniedF |]]
-}
makeConsumptionActionGIDsAndMap [ [|alreadyTookPillF |],
                                  [| pillTooFarF |],
                                  [| takePillF |],
                                 [| takePillDeniedF |]]

makePosturalActionGIDsAndMap [[| standDenied |], [| standUp |]]

