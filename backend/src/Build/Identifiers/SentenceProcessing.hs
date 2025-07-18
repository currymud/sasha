{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
module Build.Identifiers.SentenceProcessing where

import           Actions.Percieve.Look                                (manageImplicitStimulusProcess)
import           Build.Templates.Identification                       (makeProcessImplicitVerbMapsTH)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb (look)

-- processImplicitVerbMap :: Model.GameState.ProcessImplicitVerbMap
-- playerProcessImplicitVerbMap :: PlayerProcessImplicitVerbMap
-- manageImplicitStimulusProcessGID

makeProcessImplicitVerbMapsTH [| [ (look, [manageImplicitStimulusProcess]) ] |]
