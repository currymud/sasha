{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.Actions where
import           Actions.Percieve.Look                                   (agentCanSee,
                                                                          dsvActionEnabled,
                                                                          isvActionEnabled)
import           Build.BedPuzzle.Actions.Look                            (pitchBlackF)
import           Build.BedPuzzle.Actions.Objects.Pill.Look               (seePill,
                                                                          whatPill)
import           Build.Templates.Identification                          (makeDirectionalStimulusActionGIDsAndMap,
                                                                          makeImplicitStimulusActionGIDsAndMap)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Model.GameState                                         (DirectionalStimulusActionF,
                                                                          ImplicitStimulusActionF)


isaEnabledLook :: ImplicitStimulusActionF
isaEnabledLook =
  isvActionEnabled Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look

dsvEnabledLook :: DirectionalStimulusActionF
dsvEnabledLook =
  dsvActionEnabled Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look
-- implicitStimulusActionMap

makeImplicitStimulusActionGIDsAndMap [[| agentCanSee |], [| pitchBlackF |], [| isaEnabledLook |]]

-- directionalStimulusActionMap
makeDirectionalStimulusActionGIDsAndMap [[| seePill |], [| whatPill |], [| dsvEnabledLook|]]
