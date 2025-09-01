{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.Actions where
import qualified Build.BedPuzzle.Actions.Get
import           Build.BedPuzzle.Actions.Get.Constructors                (getFromSupportF,
                                                                          getObjectF)
import           Build.BedPuzzle.Actions.Player.Look                     (dsvActionEnabled,
                                                                          dsvContainerActionEnabled,
                                                                          isvActionEnabled)
import           Build.Identifiers.Objects                               (chairGID,
                                                                          pocketGID,
                                                                          robeGID)

import           Build.BedPuzzle.Actions.Get                             (getDeniedF)
import           Build.BedPuzzle.Actions.Inventory                       (defaultInventoryLookF,
                                                                          notEvenInventoryF)
import           Build.BedPuzzle.Actions.Locations.Look                  (lookF,
                                                                          pitchBlackF)
import           Build.BedPuzzle.Actions.Look                            (cannnotLookInF,
                                                                          lookAtF,
                                                                          lookInF)
import           Build.BedPuzzle.Actions.Objects.Chair.Look              (whatChairF)
import           Build.BedPuzzle.Actions.Objects.Floor.Get               (getFloorDeniedF)
import           Build.BedPuzzle.Actions.Objects.Floor.Look              (notEvenFloorF,
                                                                          seeFloorF)
import           Build.BedPuzzle.Actions.Objects.Mail.Get                (alreadyHaveMailF,
                                                                          getMailDeniedF,
                                                                          getMailDizzyF)
import           Build.BedPuzzle.Actions.Objects.Mail.Look               (notEvenMail,
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
                                                                          pocketClosedF,
                                                                          seePocketChair,
                                                                          seePocketRobeWorn,
                                                                          somethingInPocketF,
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

import           Build.Identifiers.SupportLook                           (supportLookF)
import           Build.Templates.Identification                          (makeAcquisitionActionGIDsAndMap,
                                                                          makeConsumptionActionGIDsAndMap,
                                                                          makeDirectionalStimulusActionGIDsAndMap,
                                                                          makeDirectionalStimulusContainerActionGIDsAndMap,
                                                                          makeImplicitStimulusActionGIDsAndMap,
                                                                          makePosturalActionGIDsAndMap,
                                                                          makeSomaticAccessActionGIDsAndMap)
import           Data.Text                                               (Text)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Model.GameState                                         (AcquisitionActionF,
                                                                          DirectionalStimulusActionF,
                                                                          DirectionalStimulusContainerActionF,
                                                                          ImplicitStimulusActionF,
                                                                          PlayerKey (PlayerKeyLocation),
                                                                          SystemEffectKey (SystemLocationKey, SystemPlayerKey))

isaEnabledLook :: ImplicitStimulusActionF
isaEnabledLook =
  isvActionEnabled Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dsvEnabledLook :: DirectionalStimulusActionF
dsvEnabledLook =
  dsvActionEnabled Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

dsvContainerEnabledLook :: DirectionalStimulusContainerActionF
dsvContainerEnabledLook =
  dsvContainerActionEnabled Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

seeMail :: DirectionalStimulusActionF
seeMail = lookAtF mailGID

seeRobeF :: DirectionalStimulusActionF
seeRobeF =  lookAtF robeGID

playerGetF :: AcquisitionActionF
playerGetF = Build.BedPuzzle.Actions.Get.getF

dizzyGetF :: AcquisitionActionF
dizzyGetF = Build.BedPuzzle.Actions.Get.getDeniedF

getRobeF :: AcquisitionActionF
getRobeF = getObjectF robeGID

getFromChairF :: AcquisitionActionF
getFromChairF = getFromSupportF chairGID

getFromRobeF :: AcquisitionActionF
getFromRobeF = getFromSupportF robeGID

lookAtRobeF :: DirectionalStimulusActionF
lookAtRobeF = lookAtF robeGID

lookAtPocketF :: DirectionalStimulusActionF
lookAtPocketF = lookAtF pocketGID

lookInPocketF :: DirectionalStimulusContainerActionF
lookInPocketF = lookInF pocketGID flavor
  where
    flavor :: Text
    flavor = "You look in your pocket."

seePillF :: DirectionalStimulusActionF
seePillF = ObjectDirectionalStimulusActionF

lookAtChairF :: DirectionalStimulusActionF
lookAtChairF = supportLookF chairGID flavor
  where
    flavor :: Text
    flavor = "You see a chair. It is a chair. It is not a very interesting chair, but it is a chair nonetheless."
-- Implicit Stimulus Actions with manual GID assignment

makeImplicitStimulusActionGIDsAndMap [([| pitchBlackF |], 1),
                                      ([| isaEnabledLook |], 2),
                                      ([| defaultInventoryLookF |], 3),
                                      ([| lookF |],4),
                                      ([| notEvenInventoryF |],5)]

-- Directional Stimulus Actions with manual GID assignment
makeDirectionalStimulusActionGIDsAndMap [([| seePill |], 1),
                                         ([| notEvenPill|], 2),
                                         ([| whatPill |], 3),
                                         ([| dsvEnabledLook|], 4),
                                         ([| lookAtRobeF |], 5),
                                         ([| whatTable |], 6),
                                         ([| seeTable |], 7),
                                         ([| whatChairF |], 8),
                                         ([| whatRobe|], 10),
                                         ([| notEvenRobe|], 11),
                                         ([| whatPocket |], 12),
                                         ([| notEvenPocket |], 13),
                                         ([| seeRobeF |], 16),
                                         ([| seeMail |], 17),
                                         ([| whatMail |], 18),
                                         ([| seeFloorF |], 19),
                                         ([| notEvenFloorF |], 20)]


makeDirectionalStimulusContainerActionGIDsAndMap [([| lookInPocketF |], 1),
                                                 ([| pocketClosedF |], 2),
                                                 ([| cannnotLookInF |], 3),
                                                  ([| dsvContainerEnabledLook |],4)]

makeAcquisitionActionGIDsAndMap [ ([| alreadyHaveMailF|], 1),
                                  ([| getMailDeniedF |], 2),
                                  ([| alreadyHaveRobeF |], 3),
                                  ([| playerGetF|], 4),
                                  ([| dizzyGetF |], 5),
                                  ([| getRobeF |], 6),
                                  ([| getFromChairF |], 7),
                                  ([| getFromRobeF|], 8),
                                  ([| getRobeDeniedF|], 10),
                                  ([| getMailDizzyF |], 11),
                                  ([| getDeniedF |], 12)]
                                    {-
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
