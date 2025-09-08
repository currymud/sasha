{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Robe.Get (getRobeDeniedF, alreadyHaveRobeF) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (addToInventoryM, getObjectM,
                                                modifyNarration,
                                                parseAcquisitionPhrase,
                                                updateActionConsequence)
import           Model.Core                    (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                GameComputation (GameComputation),
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase            (NounKey)
import           Relude.String                 (ToText (toText))


alreadyHaveRobeF :: AcquisitionActionF
alreadyHaveRobeF = NotGettableF haveRobe
  where
    haveRobe :: GameComputation Identity ()
    haveRobe = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You are already wearing the robe."

getRobeDeniedF :: AcquisitionActionF
getRobeDeniedF = NotGettableF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't see it. You're dizzy with a hangover from the night before. Open your eyes."
