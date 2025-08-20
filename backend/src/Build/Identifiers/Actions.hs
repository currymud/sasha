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
import           Build.Identifiers.Objects                               (chairGID,
                                                                          robeGID)

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

-- Implicit Stimulus Actions with manual GID assignment

makeImplicitStimulusActionGIDsAndMap [([| agentCanSee |], 1),
                                      ([| pitchBlackF |], 2),
                                      ([| isaEnabledLook |], 3),
                                      ([| checkInventory |], 4)]
-- makeImplicitStimulusActionGIDsAndMap [
 --                                     ([| pitchBlackF |],1),
 --                                     ([| isaEnabledLook |], 2)]

-- Directional Stimulus Actions with manual GID assignment
makeDirectionalStimulusActionGIDsAndMap [([| seePill |], 1),
                                         ([| notEvenPill|], 2),
                                         ([| whatPill |], 3),
                                         ([| dsvEnabledLook|], 4),
                                         ([| lookAt |], 5),
                                         ([| whatTable |], 6),
                                         ([| seeTable |], 7),
                                         ([| whatChairF |], 8),
                                         ([| seeChairF|], 9),
                                         ([| whatRobe|], 10),
                                         ([| notEvenRobe|], 11),
                                         ([| whatPocket |], 12),
                                         ([| notEvenPocket |], 13),

                                         ([| seePocketChair |], 14),
                                         ([| seePocketRobeWorn |], 15),
                                         ([| seeRobeChair |], 16),
                                         ([| seeMail |], 17),
                                         ([| whatMail |], 18),

                                         ([| seeFloorF |], 19),
                                         ([| notEvenFloorF |], 20)]

makeAcquisitionActionGIDsAndMap [ ([| alreadyHaveMailF|], 1),
                                  ([| getMailDeniedF |], 2),
                                  ([| alreadyHaveRobeF |], 3),
                                  ([| playerGetF|], 4),
                                  ([| dizzyGetF |], 5),
                                  ([| getRobeF |], 6),
                                  ([| getFromChairF |], 7),
                                  ([| getFromRobeF|], 8),
                                  ([| robeCollectedF|], 9),
                                  ([| getRobeDeniedF|], 10),
                                  ([| getMailDizzyF |], 11)]
                                    {-
                                 [| getDenied |],
                                 [| getPillDeniedF |],
                                 [| alreadyHaveRobeF |],
                                [| getFloorDeniedF |]]
-}

makeConsumptionActionGIDsAndMap [([| takePillF |], 1),
                                 ([| takePillDeniedF |], 2),
                                 ( [| pillTooFarF |],3)]

-- Somatic Access Actions with manual GID assignment
makeSomaticAccessActionGIDsAndMap [([| openEyes |], 1),
                                   ([| openEyesDenied |], 2)]

-- Postural Actions with manual GID assignment
makePosturalActionGIDsAndMap [([| standUp |], 1),
                              ([| standDenied |], 2)]
