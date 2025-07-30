{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.Actions where
import           Actions.Percieve.Look                                (actionEnabled,
                                                                       agentCanSee)
import           Build.BedPuzzle.Actions.Look                         (pitchBlackF)
import           Build.BedPuzzle.Actions.Objects.Pill.Look            (seePill,
                                                                       whatPill)
import           Build.Templates.Identification                       (makeDirectionalStimulusActionGIDsAndMap,
                                                                       makeImplicitStimulusActionGIDsAndMap)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)
import           Model.GameState                                      (ImplicitStimulusActionF)


enabledLook :: ImplicitStimulusActionF
enabledLook = actionEnabled look
-- implicitStimulusActionMap

makeImplicitStimulusActionGIDsAndMap [[| agentCanSee |], [| pitchBlackF |], [| enabledLook |]]

-- directionalStimulusActionMap
makeDirectionalStimulusActionGIDsAndMap [[| seePill |], [| whatPill |]]
