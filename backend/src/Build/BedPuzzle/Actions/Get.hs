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
import           Debug.Trace                   (trace)
import           GameState                     (addToInventoryM, getObjectM,
                                                getPlayerLocationM, getPlayerM,
                                                modifyNarration,
                                                modifySpatialRelationshipsForObjectM,
                                                parseAcquisitionPhrase,
                                                parseSupportPhrase)
import           GameState.ActionManagement    (lookupAcquisition,
                                                lookupAcquisitionVerbPhrase)
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
                                                Effect (AcquisitionVerbEffect, ConsumptionEffect, DirectionalStimulusEffect, NegativePosturalEffect, PositivePosturalEffect),
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
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb)
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
    getit searchStrategy avp =
      let (_ophrase, nounKey) = parseAcquisitionPhrase avp
      in case avp of
        SimpleAcquisitionVerbPhrase _verb objectPhrase -> do
        -- Parse the object phrase to get the noun key

        -- Use search strategy to find target object and its container/supporter
          maybeResult <- searchStrategy nounKey
          case maybeResult of
            Just (objectGID, containerGID) -> do
            -- Coordinate the handoff between source and target
              doGet containerGID objectGID avp (lookupAcquisition . extractVerb)
            Nothing -> do
            -- Object not found or not accessible
              modifyNarration $ updateActionConsequence "You don't see that here."

        AcquisitionVerbPhrase _verb objectPhrase _sourceMarker supportPhrase -> do
        -- Parse both the target and source from the phrase
          let sourceNounKey = parseSupportPhrase supportPhrase

        -- Find target and source GIDs using location semantic map
          playerLocation <- getPlayerLocationM
          let objectSemanticMap = _objectSemanticMap playerLocation

        -- Look up target object
          targetResult <- case Data.Map.Strict.lookup nounKey objectSemanticMap of
            Just objSet | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
            _ -> pure Nothing

        -- Look up source object
          sourceResult <- case Data.Map.Strict.lookup sourceNounKey objectSemanticMap of
            Just objSet | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
            _ -> pure Nothing

          case (targetResult, sourceResult) of
            (Just targetGID, Just sourceGID) -> do
            -- Verify the spatial relationship exists
              world <- gets _world
              let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
              case Data.Map.Strict.lookup targetGID spatialMap of
                Just relationships -> do
                  let isContainedInSource = any (\case
                        ContainedIn oid -> oid == sourceGID
                        SupportedBy oid -> oid == sourceGID
                        _ -> False) (Data.Set.toList relationships)
                  if isContainedInSource
                    then doGet sourceGID targetGID avp lookupAcquisitionVerbPhrase
                    else modifyNarration $ updateActionConsequence "That's not in there."
                Nothing -> modifyNarration $ updateActionConsequence "That's not in there."
            (Nothing, _) -> modifyNarration $ updateActionConsequence "You don't see that here."
            (_, Nothing) -> modifyNarration $ updateActionConsequence "You don't see that container here."
doGet :: GID Object
          -> GID Object
          -> AcquisitionVerbPhrase
          -> (AcquisitionVerbPhrase -> ActionManagementFunctions -> Maybe (GID AcquisitionActionF))
          -> GameComputation Identity ()
doGet sourceGID targetGID avp lookupF = do
 actionMap <- asks (_acquisitionActionMap . _actionMaps)
 sourceObj <- getObjectM sourceGID
 targetObj <- getObjectM targetGID

 let sourceActionMgmt = _objectActionManagement sourceObj
     removedFromRes = case lookupF avp sourceActionMgmt of
       Just sourceActionGID ->
         case Data.Map.Strict.lookup sourceActionGID actionMap of
            Just (LosesObjectF actionFunc) -> actionFunc targetGID
            Just (CollectedF _) ->
              Left $ modifyNarration $ updateActionConsequence "Source object should use LosesObjectF constructor but uses CollectedF"
            Just (AcquisitionActionF _) ->
              Left $ modifyNarration $ updateActionConsequence "Source object should use LosesObjectF constructor but uses AcquisitionActionF"
            Nothing ->
              Left $ modifyNarration $ updateActionConsequence "Source object's acquisition action not found in action map"
       Nothing ->
         Left $ modifyNarration $ updateActionConsequence "Source object has no acquisition action"

     targetActionMgmt = _objectActionManagement targetObj
     addedToRes = case lookupF avp targetActionMgmt of
       Just targetActionGID -> do
         trace ("DEBUG: Found target object action GID: " ++ show targetActionGID) $ pure ()
         case Data.Map.Strict.lookup targetActionGID actionMap of
           Just (CollectedF actionFunc) -> do
             trace ("DEBUG: Executing CollectedF action for GID: " ++ show targetActionGID) $
               actionFunc
           Just (LosesObjectF _) ->
             trace ("DEBUG: Found LosesObjectF for target GID: " ++ show targetActionGID) $  -- ADD THIS

               Left $ modifyNarration $ updateActionConsequence "Target object should use CollectedF constructor but uses LosesObjectF"
           Just (AcquisitionActionF _) ->
             Left $ modifyNarration $ updateActionConsequence "Target object should use CollectedF constructor but uses AcquisitionActionF"
           Nothing ->
             Left $ modifyNarration $ updateActionConsequence "Target object's acquisition action not found in action map"
       Nothing ->
         Left $ modifyNarration $ updateActionConsequence "Target object has no acquisition action for this phrase"

 either id id $ do
   comp1 <- removedFromRes
   comp2 <- addedToRes
   pure $ comp1 >> comp2
extractVerb :: AcquisitionVerbPhrase -> AcquisitionVerb
extractVerb (SimpleAcquisitionVerbPhrase verb _) = verb
extractVerb (AcquisitionVerbPhrase verb _ _ _)   = verb
