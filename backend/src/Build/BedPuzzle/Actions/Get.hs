{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (getF,getDeniedF) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (gets, modify')
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Set                      (Set, delete, elemAt, filter,
                                                insert, map, null, toList)
import           Data.Text                     (Text)
import           GameState                     (addToInventoryM, getObjectM,
                                                getPlayerM, modifyNarration,
                                                modifySpatialRelationshipsForObjectM,
                                                parseAcquisitionPhrase)
import           GameState.ActionManagement    (lookupAcquisition)
import           GameState.Perception          (updatePerceptionMapM)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF),
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
                                                GameState (_player, _world),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_objectActionManagement),
                                                Player (_playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                SearchStrategy,
                                                SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Nouns (SupportPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase),
                                                ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Model.Parser.GCase            (NounKey)

getDeniedF :: AcquisitionActionF
getDeniedF = AcquisitionActionF (const (const denied))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

getF :: AcquisitionActionF
getF = AcquisitionActionF getit
  where
    getit :: SearchStrategy -> AcquisitionVerbPhrase -> GameComputation Identity ()
    getit loc avp = undefined

doGet :: GID Object -> GID Object -> AcquisitionVerbPhrase -> GameComputation Identity ()
doGet sourceGID targetGID avp = do
  -- Role 1: Execute source object's RemovedFromF action
  actionMap <- asks (_acquisitionActionMap . _actionMaps)

  sourceObj <- getObjectM sourceGID
  let sourceActionMgmt = _objectActionManagement sourceObj
      removedFromRes = case lookupAcquisition avp sourceActionMgmt of
        Just sourceActionGID -> do
         case Data.Map.Strict.lookup sourceActionGID actionMap of
             Just (LosesObjectF actionFunc) -> do
               actionFunc targetGID
             Just _ -> error "Source object should use RemovedFromF constructor"
             Nothing -> error "Source object's acquisition action not found in action map"
        Nothing -> error "Source object has no acquisition action for this phrase"
  targetObj <- getObjectM targetGID
  let targetActionMgmt = _objectActionManagement targetObj
      addedToRes = case lookupAcquisition avp targetActionMgmt of
        Just targetActionGID -> do
          case Data.Map.Strict.lookup targetActionGID actionMap of
            Just (CollectedF actionFunc) -> actionFunc
            Just _ -> error "Target object should use AcquiredFromF constructor"
            Nothing -> error "Target object's acquisition action not found in action map"
        Nothing -> error "Target object has no acquisition action for this phrase"
  either id id(removedFromRes >> addedToRes)


