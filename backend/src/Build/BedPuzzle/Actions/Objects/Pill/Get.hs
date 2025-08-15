{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Pill.Get (getPillDeniedF,alreadyHavePillF) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (addToInventoryM,
                                                modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (CollectedF),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase            (NounKey)
import           Relude.String                 (ToText (toText))
  {-

when doing an actual get attempt, use helper functions in top level

the functions here answer different questions. Are you capable of getting the robe

  -}

alreadyHavePillF :: AcquisitionActionF
alreadyHavePillF = CollectedF havePill
  where
    havePill :: Either (GameComputation Identity ()) (GameComputation Identity ())
    havePill = Left $ modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already have the pill in your inventory."

getPillDeniedF :: AcquisitionActionF
getPillDeniedF = CollectedF denied
  where
    denied :: Either (GameComputation Identity ()) (GameComputation Identity ())
    denied = Left $ modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"
