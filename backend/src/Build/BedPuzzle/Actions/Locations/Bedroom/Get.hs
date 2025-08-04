{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Locations.Bedroom.Get (get,getDenied) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (AcquisitionActionF (AcquisitionActionF, RemovedFromF),
                                         GameComputation,
                                         Location (_locationActionManagement, _objectSemanticMap),
                                         updateActionConsequence)
import           Model.Parser.GCase     (NounKey)


getDenied :: AcquisitionActionF
getDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

get :: AcquisitionActionF
get =  RemovedFromF getit
  where
    getit :: Location -> NounKey -> GameComputation Identity ()
    getit loc nounKey  = pure ()
