{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (gets, modify')
import qualified Data.Map.Strict
import           Data.Set                      (Set, elemAt, filter, insert,
                                                null, toList)
import           Data.Text                     (Text)
import           GameState                     (getObjectM, getPlayerM,
                                                modifyNarration,
                                                parseAcquisitionPhrase)
import           GameState.ActionManagement    (lookupAcquisition)
import           GameState.Perception          (updatePerceptionMapM)
import           Model.GameState               (AcquisitionActionF (AcquiredFromF, AcquisitionActionF, RemovedFromF),
                                                ActionEffectKey (ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap, _actionEffectMap),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                                ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey, NPManagementKey, PPManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect, NegativePosturalEffect, PositivePosturalEffect),
                                                GameComputation,
                                                GameState (_player),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_objectActionManagement),
                                                Player (_actionKeyMap, _playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase))

getDenied :: AcquisitionActionF
getDenied = AcquisitionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

get :: AcquisitionActionF
get = AcquisitionActionF getit
  where
    getit :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity ()
    getit loc actionEffectMap avp = pure ()

executeObjectGet :: Location -> ActionEffectMap -> AcquisitionVerbPhrase -> GameComputation Identity (Either (GameComputation Identity ()) (GameComputation Identity ()))
executeObjectGet loc actionEffectMap avp = do
  let (objectPhrase, nounKey) = parseAcquisitionPhrase avp
  case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      let oid = Data.Set.elemAt 0 objSet
      obj <- getObjectM oid
      let objectActionMgmt = _objectActionManagement obj
      case lookupAcquisition avp objectActionMgmt of
        Just objectActionGID -> do
          actionMap <- asks (_acquisitionActionMap . _actionMaps)
          case Data.Map.Strict.lookup objectActionGID actionMap of
            Just (AcquisitionActionF objectAction) -> do
              pure $ Left $ objectAction loc actionEffectMap avp
            Just (AcquiredFromF objectAcquiredAction) -> do
              objectAcquiredAction loc avp
            Just (RemovedFromF objectRemovedAction) -> do
              objectRemovedAction loc avp
            Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence "Object action not found in action map"
        Nothing ->
          pure $ Right $ modifyNarration $ updateActionConsequence "Object allows general acquisition"
    _ -> pure $ Left $ modifyNarration $ updateActionConsequence "You don't see that here."
