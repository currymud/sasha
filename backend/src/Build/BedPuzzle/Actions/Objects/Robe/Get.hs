{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Robe.Get (getRobe,getRobeDenied,alreadyHaveRobe) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (addToInventoryM, getObjectM,
                                                modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF),
                                                GameComputation (GameComputation),
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase            (NounKey)
import           Relude.String                 (ToText (toText))

alreadyHaveRobe :: AcquisitionActionF
alreadyHaveRobe = AcquisitionActionF (const (const haveRobe))
  where
    haveRobe :: GameComputation Identity ()
    haveRobe = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You are already wearing the robe."

getRobeDenied :: AcquisitionActionF
getRobeDenied = AcquiredFromF (Left denied)
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't see it. You're dizzy with a hangover from the night before. Open your eyes."

getRobe :: AcquisitionActionF
getRobe = AcquiredFromF getit
  where
    getit :: Either (GameComputation Identity ()) (GameComputation Identity ())
    getit = Left (pure ())
