{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (getF,getDeniedF) where
import           Control.Monad.Identity                           (Identity)
import           Control.Monad.Reader                             (asks)
import           Control.Monad.State                              (gets,
                                                                   modify')
import           Data.Map.Strict                                  (Map)
import qualified Data.Map.Strict
import           Data.Set                                         (Set, delete,
                                                                   elemAt,
                                                                   filter,
                                                                   insert, map,
                                                                   null, toList)
import           Data.Text                                        (Text)
import           Debug.Trace                                      (trace)
import           GameState                                        (addToInventoryM,
                                                                   getObjectM,
                                                                   getPlayerLocationM,
                                                                   getPlayerM,
                                                                   modifyNarration,
                                                                   modifySpatialRelationshipsForObjectM,
                                                                   parseAcquisitionPhrase,
                                                                   parseSupportPhrase)
import           GameState.ActionManagement                       (findAAKey,
                                                                   findAVKey,
                                                                   lookupAcquisition,
                                                                   lookupAcquisitionVerbPhrase,
                                                                   processEffectsFromRegistry)
import           GameState.Perception                             (updatePerceptionMapM)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.GameState                                  (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                                   AcquisitionRes (Complete, Simple),
                                                                   AcquisitionVerbActionMap,
                                                                   ActionEffectKey (ObjectKey, PlayerKey),
                                                                   ActionEffectMap (ActionEffectMap, _actionEffectMap),
                                                                   ActionKey (AcquisitionalActionKey),
                                                                   ActionKeyMap (ActionKeyMap, _unActionKeyMap),
                                                                   ActionManagement (AAManagementKey, AVManagementKey, CAManagementKey, DSAManagementKey, NPManagementKey, PPManagementKey),
                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                   ActionMaps (_acquisitionActionMap),
                                                                   CompleteAcquisitionRes (CompleteAcquisitionRes, _caObjectKey, _caObjectPhrase, _caSupportKey, _caSupportPhrase),
                                                                   Config (_actionMaps),
                                                                   Effect (AcquisitionVerbEffect, ConsumptionEffect, DirectionalStimulusEffect, NegativePosturalEffect, PositivePosturalEffect),
                                                                   GameComputation,
                                                                   GameState (_player, _world),
                                                                   Location (_locationActionManagement, _objectSemanticMap),
                                                                   Object (_objectActionManagement),
                                                                   Player (_playerActions),
                                                                   PlayerKey (PlayerKeyObject),
                                                                   SearchStrategy,
                                                                   SimpleAcquisitionRes (SimpleAcquisitionRes, _saObjectKey, _saObjectPhrase),
                                                                   SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                   World (_spatialRelationshipMap),
                                                                   updateActionConsequence)
import           Model.GID                                        (GID)
import           Model.Parser.Atomics.Verbs                       (AcquisitionVerb)
import           Model.Parser.Composites.Nouns                    (SupportPhrase)
import           Model.Parser.Composites.Verbs                    (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase),
                                                                   ConsumptionVerbPhrase (ConsumptionVerbPhrase))
import           Model.Parser.GCase                               (NounKey)

getDeniedF :: AcquisitionActionF
getDeniedF = NotGettableF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

getF :: AcquisitionActionF
getF = AcquisitionActionF getit
  where
    getit :: ActionKey
               -> AcquisitionVerbActionMap
               -> SearchStrategy
               -> AcquisitionVerbPhrase
               -> GameComputation Identity ()
    getit actionKey actionMap searchStrategy avp = do
      case ares of
        Simple (SimpleAcquisitionRes {..}) -> do
          maybeResult <- searchStrategy _saObjectKey
          case maybeResult of
            Just (objectGID, containerGID) -> do
              -- Coordinate the handoff between source and target
              objectActionManagement <- _objectActionManagement <$> getObjectM objectGID
              case findAVKey get objectActionManagement of
                Nothing -> error $ "Programmer Error: getF - Object " ++ show objectGID ++ " does not have a 'get' action."
                Just actionGID -> do
                  case Data.Map.Strict.lookup actionGID actionMap of
                    Nothing -> error $ "Programmer Error: getF - No acquisition action found for GID: " ++ show actionGID
                    Just (CollectedF actionFunc) ->
                       case actionFunc of
                         Left notGetF -> notGetF >> processEffectsFromRegistry actionKey
                         Right goGetF -> do
                           supportActionManagement <- _objectActionManagement <$> getObjectM containerGID
                           pure ()
                    _ -> error $ "Programmer Error: getF - Action for GID: " ++ show actionGID ++ " is not a CollectedF."
--              _ <- doGet containerGID objectGID avp
              pure ()
            Nothing -> do
              -- Object not found or not accessible
              modifyNarration $ updateActionConsequence "You don't see that here."
          pure ()
        Complete (CompleteAcquisitionRes {..}) -> pure ()
      pure ()
      where
        ares = parseAcquisitionPhrase avp

      {-
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
            e- Object not found or not accessible
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
-}
  {-
checkGettable :: GID Object -> AcquisitionVerbPhrase -> GameComputation Identity (Either (ActionKey -> GameComputation Identity ()) (ActionKey -> GID Object -> AcquisitionVerbPhrase -> GameComputation Identity ()))
checkGettable targetGID avp = do
  -- Get the target object and find its acquisition action
  targetObj <- getObjectM targetGID
  let targetActionMgmt = _objectActionManagement targetObj
  actionMap <- asks (_acquisitionActionMap . _actionMaps)
  case Data.Map.Strict.lookup targetActionGID actionMap of
    Just (NotGettableF actionFunc) ->
          -- Object is not gettable - return the NotGettable function
     pure $ Left actionFunc
     Just (CollectedF _) ->
          -- Object is gettable - return continuation function
       error "Programmer Error: CollectedF should not be used for the thing being gotten."
     Just (LosesObjectF _) ->
          -- Object is gettable - return continuation function
     pure $ Right proceedWithSpatialValidation
        Just (AcquisitionActionF _) ->
          pure $ Left $ \_ -> modifyNarration $ updateActionConsequence "Object has incorrect action type."
        Nothing ->
          pure $ Left $ \_ -> modifyNarration $ updateActionConsequence "Object's action not

-- Step 1: Complete doGet implementation with internal findKeys function
doGet :: GID Object
          -> GID Object
          -> AcquisitionVerbPhrase
          -> GameComputation Identity [ActionKey]
doGet sourceGID targetGID avp = do
  -- Get the actual objects to access their action management
  sourceObj <- getObjectM sourceGID
  targetObj <- getObjectM targetGID

  let sourceActionMgmt = _objectActionManagement sourceObj
      targetActionMgmt = _objectActionManagement targetObj
  -- Collect ActionKeys from both objects
  let sourceKeys = findKeys avp sourceActionMgmt
      targetKeys = findKeys avp targetActionMgmt

  -- Return combined keys from both objects
  pure (sourceKeys ++ targetKeys)
-}
findKeys :: AcquisitionVerbPhrase -> ActionManagementFunctions -> [ActionKey]
findKeys phrase (ActionManagementFunctions actions) =
  let phraseKeys = [AcquisitionalActionKey gid | AAManagementKey p gid <- Data.Set.toList actions, p == phrase]
      verb = extractVerb phrase
      verbKeys = [AcquisitionalActionKey gid | AVManagementKey v gid <- Data.Set.toList actions, v == verb]
  in phraseKeys ++ verbKeys

extractVerb :: AcquisitionVerbPhrase -> AcquisitionVerb
extractVerb (SimpleAcquisitionVerbPhrase verb _) = verb
extractVerb (AcquisitionVerbPhrase verb _ _ _)   = verb
