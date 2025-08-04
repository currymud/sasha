{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Pill.Get (get,getDenied) where
import           Control.Monad.Identity        (Identity)
import           Data.Text                     (Text)
import           GameState                     (modifyNarration)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                GameComputation,
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object, updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase))
import           Model.Parser.GCase            (NounKey)


getDenied :: AcquisitionActionF
getDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

get :: AcquisitionActionF
get =  AcquiredFromF getit
  where
    getit :: Object -> AcquisitionVerbPhrase -> GameComputation Identity ()
    getit obj avp  = pure ()
