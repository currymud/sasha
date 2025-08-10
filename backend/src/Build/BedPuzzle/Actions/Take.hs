{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Take (takeDenied) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (modify')
import qualified Data.Map.Strict
import           Data.Set                      (Set, elemAt, null, toList)
import           Data.Text                     (Text)
import           GameState                     (getObjectM, modifyNarration,
                                                modifyObjectActionManagementM,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionEffectKey (ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap, _actionEffectMap),
                                                ActionMaps (_consumptionActionMap),
                                                Config (_actionMaps),
                                                ConsumptionActionF (ConsumptionActionF, _consumptionAction),
                                                Effect (AcquisitionEffect, DirectionalStimulusEffect),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_objectActionManagement),
                                                Player (_actionKeyMap, _playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase),
                                                Imperative (ConsumptionVerbPhrase'))
import           Prelude                       hiding (take)

takeDenied :: ConsumptionActionF
takeDenied = ConsumptionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"
      {-
take :: ConsumptionActionF
take = ConsumptionActionF getit
-}

