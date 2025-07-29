{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.Actions where
import           Actions.Percieve.Look                     (agentCanSee)
import           Build.BedPuzzle.Actions.Look              (pitchBlackF)
import           Build.BedPuzzle.Actions.Objects.Pill.Look (seePill, whatPill)
import           Build.Templates.Identification            (makeDirectionalStimulusActionGIDsAndMap,
                                                            makeImplicitStimulusActionGIDsAndMap)
-- implicitStimulusActionMap

makeImplicitStimulusActionGIDsAndMap [[| agentCanSee |], [| pitchBlackF |]]

-- directionalStimulusActionMap
makeDirectionalStimulusActionGIDsAndMap [[| seePill |], [| whatPill |]]
