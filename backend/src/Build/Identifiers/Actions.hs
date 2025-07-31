{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.Actions where
import           Actions.Manipulate.SomaticAccess.Open                   (savActionEnabled)
import           Actions.Percieve.Look                                   (agentCanSee,
                                                                          dsvActionEnabled,
                                                                          isvActionEnabled,
                                                                          lookAt)
import           Build.BedPuzzle.Actions.Look                            (pitchBlackF)
import           Build.BedPuzzle.Actions.Objects.Pill.Look               (notEvenPill,
                                                                          seePill,
                                                                          whatPill)
import           Build.BedPuzzle.Actions.Objects.Table.Look              (seeTable,
                                                                          whatTable)
import           Build.BedPuzzle.Actions.Open                            (openEyes,
                                                                          openEyesDenied)
import           Build.Templates.Identification                          (makeDirectionalStimulusActionGIDsAndMap,
                                                                          makeImplicitStimulusActionGIDsAndMap,
                                                                          makeSomaticAccessActionGIDsAndMap)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (open)
import           Model.GameState                                         (DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF,
                                                                          SomaticAccessActionF)


isaEnabledLook :: ImplicitStimulusActionF
isaEnabledLook =
  isvActionEnabled Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dsvEnabledLook :: DirectionalStimulusActionF
dsvEnabledLook =
  dsvActionEnabled Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

somaticAccessOpenEyes :: SomaticAccessActionF
somaticAccessOpenEyes =
  savActionEnabled Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs.open
-- implicitStimulusActionMap

makeImplicitStimulusActionGIDsAndMap [[| agentCanSee |], [| pitchBlackF |], [| isaEnabledLook |]]

-- directionalStimulusActionMap
makeDirectionalStimulusActionGIDsAndMap [[| seePill |], [| notEvenPill|] , [| whatPill |], [| dsvEnabledLook|], [| lookAt |], [| whatTable |], [| seeTable |] ]

makeSomaticAccessActionGIDsAndMap [[|openEyesDenied |], [| openEyes|], [| somaticAccessOpenEyes |]]
