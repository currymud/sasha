{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (getF,getDeniedF) where
import           Control.Monad.Identity                           (Identity)
import           Control.Monad.Reader                             (asks)
import           Control.Monad.State                              (gets,
                                                                   modify')
import           Data.Kind                                        (Type)
import           Data.Map.Strict                                  (Map)
import qualified Data.Map.Strict
import           Data.Set                                         (Set, delete,
                                                                   elemAt,
                                                                   filter,
                                                                   insert, map,
                                                                   null, toList)
import           Data.Text                                        (Text, pack)
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
                                                                   processAllEffects,
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
                                                                   CoordinationResult (CoordinationResult),
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
  {-
                Just actionGID -> do
                  case Data.Map.Strict.lookup actionGID actionMap of
                    Nothing -> error $ "Programmer Error: getF - No acquisition action found for GID: " ++ show actionGID
                    Just (CollectedF actionFunc) ->
                       case actionFunc of
                         Left notGetF -> notGetF >> processEffectsFromRegistry actionKey
                         Right goGetF -> do
                           supportActionManagement <- _objectActionManagement <$> getObjectM containerGID
                           case findAVKey get supportActionManagement of
                             Nothing -> error $ "Programmer Error: getF - Container " ++ show containerGID ++ " does not have a 'get' action."
                             Just supportActionGID -> do
                               case Data.Map.Strict.lookup supportActionGID actionMap of
                                 Nothing -> error $ "Programmer Error: getF - No acquisiition action found for container GID: " ++ show supportActionGID
                                 Just (LosesObjectF supportActionF) -> do
                                   case supportActionF objectGID of
                                     Left _errorF    -> pure ()
                                     Right _successF -> pure ()
                                 Just _ -> error $ "Programmer Error: getF - Action for GID: " ++ show supportActionGID ++ " is not a LosesObjectF."
                               -- actionF objectGID
                           pure ()
                    _ -> error $ "Programmer Error: getF - Action for GID: " ++ show actionGID ++ " is not a CollectedF."
-}
  {-
         maybeResult <- searchStrategy _saObjectKey
          case maybeResult of
            Nothing -> modifyNarration $ updateActionConsequence "You don't see that here."
            Just (objectGID, containerGID) -> do
              -- Coordinate the handoff between source and target
              objectActionManagement <- _objectActionManagement <$> getObjectM objectGID
-}
getDeniedF :: AcquisitionActionF
getDeniedF = NotGettableF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"
-- processAllEffects :: ActionEffectMap -> GameComputation Identity ()
-- processEffectsFromRegistry :: ActionKey -> GameComputation Identity ()

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
          osValidation <- validateObjectSearch searchStrategy _saObjectKey
          case osValidation of
            Left err -> handleAcquisitionError err
            Right (objectGID, containerGID) -> do
              objectActionLookup <- lookupAcquisitionAction objectGID actionMap ("Object " <> (Data.Text.pack . show) objectGID <> ":")
              case objectActionLookup of
                Left err-> handleAcquisitionError err
                Right (CollectedF objectActionF) -> do
                  containerActionLookup <- lookupAcquisitionAction containerGID actionMap ("Container " <> (Data.Text.pack . show) containerGID <> ":")
                  case containerActionLookup of
                    Left err -> handleAcquisitionError err
                    Right (LosesObjectF containerActionF) -> do
                      (CoordinationResult playerGetObjectF objectEffects) <- objectActionF
                      (CoordinationResult containerRemoveObjectF containerEffects) <- containerActionF objectGID
                      let allEffects = actionKey:(objectEffects <> containerEffects)
                      mapM_ processEffectsFromRegistry allEffects >> containerRemoveObjectF >> playerGetObjectF
                      pure ()
                    Right _ -> handleAcquisitionError $ InvalidActionType $ "Container " <> (Data.Text.pack . show) containerGID <> " does not have a LosesObjectF action."
                Right _ -> handleAcquisitionError $ ObjectNotGettable $ "Object " <> (Data.Text.pack . show) objectGID <> " is not gettable."
              pure ()
        Complete (CompleteAcquisitionRes {..}) -> pure ()
      pure ()
      where
        ares = parseAcquisitionPhrase avp

validateObjectSearch :: SearchStrategy -> NounKey -> GameComputation Identity (Either AcquisitionError (GID Object, GID Object))
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing -> pure $ Left $ ObjectNotFound "You don't see that here."
    Just (objectGID, containerGID) -> pure $ Right (objectGID, containerGID)

lookupAcquisitionAction :: GID Object
                             -> AcquisitionVerbActionMap
                             -> Text
                             -> GameComputation Identity (Either AcquisitionError AcquisitionActionF)
lookupAcquisitionAction objectGID actionMap contextDescription = do
  actionMgmt <- _objectActionManagement <$> getObjectM objectGID
  case findAVKey get actionMgmt of
    Nothing -> pure $ Left $ ContainerMissingAction $ contextDescription <> " " <> (Data.Text.pack . show) objectGID <> " does not have a 'get' action."
    Just actionGID ->
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> pure $ Left $ InvalidActionType $ "No acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> pure $ Right action

type AcquisitionError :: Type
data AcquisitionError
  = ObjectNotFound Text
  | ObjectNotGettable Text
  | ContainerMissingAction Text
  | InvalidActionType Text
  | SpatialValidationFailed Text

handleAcquisitionError :: AcquisitionError -> GameComputation Identity ()
handleAcquisitionError err = modifyNarration $ updateActionConsequence $ case err of
  ObjectNotFound msg          -> msg
  ObjectNotGettable msg       -> msg
  ContainerMissingAction msg  -> msg
  InvalidActionType msg       -> msg
  SpatialValidationFailed msg -> msg
    {-
findKeys :: AcquisitionVerbPhrase -> ActionManagementFunctions -> [ActionKey]
findKeys phrase (ActionManagementFunctions actions) =
  let phraseKeys = [AcquisitionalActionKey gid | AAManagementKey p gid <- Data.Set.toList actions, p == phrase]
      verb = extractVerb phrase
      verbKeys = [AcquisitionalActionKey gid | AVManagementKey v gid <- Data.Set.toList actions, v == verb]
  in phraseKeys ++ verbKeys
-}
extractVerb :: AcquisitionVerbPhrase -> AcquisitionVerb
extractVerb (SimpleAcquisitionVerbPhrase verb _) = verb
extractVerb (AcquisitionVerbPhrase verb _ _ _)   = verb
