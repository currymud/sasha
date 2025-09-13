{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Player.Get (getF,getDeniedF) where
import           ConstraintRefinement.Actions.Utils               (AcquisitionError (ContainerMissingAction, InvalidActionType, ObjectNotFound, ObjectNotGettable, SpatialValidationFailed),
                                                                   handleAcquisitionError)
import           Control.Monad.Identity                           (Identity)
import           Control.Monad.State                              (gets)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                                        (Text, pack)
import           Debug.Trace                                      (trace)
import           GameState                                        (getObjectM,
                                                                   getPlayerLocationM,
                                                                   modifyNarration,
                                                                   parseAcquisitionPhrase,
                                                                   updateActionConsequence)
import           GameState.ActionManagement                       (findAVKey)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Actions.Results                            (AcquisitionRes (Complete, Simple),
                                                                   CompleteAcquisitionRes (CompleteAcquisitionRes, _caObjectKey, _caObjectPhrase, _caSupportKey, _caSupportPhrase),
                                                                   SimpleAcquisitionRes (SimpleAcquisitionRes, _saObjectKey))
import           Model.Core                                       (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                                   AcquisitionVerbActionMap,
                                                                   ActionEffectKey,
                                                                   FinalizeAcquisitionF,
                                                                   GameComputation,
                                                                   GameState (_world),
                                                                   Location (_objectSemanticMap),
                                                                   Object (_objectActionManagement),
                                                                   SearchStrategy,
                                                                   SpatialRelationship (ContainedIn, SupportedBy),
                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                   World (_spatialRelationshipMap))
import           Model.GID                                        (GID)
import           Model.Parser.Composites.Verbs                    (AcquisitionVerbPhrase)
import           Model.Parser.GCase                               (NounKey)

getDeniedF :: AcquisitionActionF
getDeniedF = NotGettableF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

-- ToDo: refactor to remove dead code.
getF :: AcquisitionActionF
getF = AcquisitionActionF getit
  where
    getit :: ActionEffectKey
               -> AcquisitionVerbActionMap
               -> SearchStrategy
               -> AcquisitionVerbPhrase
               -> FinalizeAcquisitionF
               -> GameComputation Identity ()
    getit actionKey actionMap searchStrategy avp finalize = do
      case ares of
        Simple (SimpleAcquisitionRes {..}) -> do
          osValidation <- validateObjectSearch searchStrategy _saObjectKey
          case osValidation of
            Left err' -> handleAcquisitionError err'
            Right (objectGID, containerGID) -> do
              objectActionLookup <- lookupAcquisitionAction objectGID actionMap
              case objectActionLookup of
                Left err' -> handleAcquisitionError err'
                Right (NotGettableF objectNotGettableF) -> objectNotGettableF
                Right (CollectedF objectActionF) -> do
                  containerActionLookup <- lookupAcquisitionAction containerGID actionMap
                  case containerActionLookup of
                    Left err' -> handleAcquisitionError err'
                    Right (NotGettableF cannotGetFromF) -> cannotGetFromF
                    Right (LosesObjectF containerActionF) -> finalize actionKey containerGID objectGID objectActionF containerActionF
                    Right _ -> handleAcquisitionError $ InvalidActionType $ "Container " <> (Data.Text.pack . show) containerGID <> " does not have a LosesObjectF action."
                Right _ -> handleAcquisitionError $ ObjectNotGettable $ "Object " <> (Data.Text.pack . show) objectGID <> " is not gettable."
        Complete (CompleteAcquisitionRes {..}) -> do
          -- Find both objects directly
          objectResult <- findObjectByKey _caObjectKey
          supportResult <- findObjectByKey _caSupportKey

          case (objectResult, supportResult) of
            (Nothing, _) -> handleAcquisitionError $ ObjectNotFound "You don't see that object here."
            (_, Nothing) -> handleAcquisitionError $ ObjectNotFound "You don't see that support here."
            (Just objectGID, Just supportGID) -> do
              -- Validate the object is actually on/in the support
              world <- gets _world
              let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
              case Data.Map.Strict.lookup objectGID spatialMap of
                Nothing -> handleAcquisitionError $ SpatialValidationFailed "Object has no spatial relationships"
                Just relationships -> do
                  let isOnSupport = any (\case
                        SupportedBy sid -> sid == supportGID
                        ContainedIn cid -> cid == supportGID
                        _ -> False) (Data.Set.toList relationships)

                  if not isOnSupport
                    then handleAcquisitionError $ SpatialValidationFailed $
                      "The " <> (Data.Text.pack . show) objectGID <>
                      " is not on the " <> (Data.Text.pack . show) supportGID
                    else do
                      -- Now proceed with the standard lookups
                      objectActionLookup <- lookupAcquisitionAction objectGID actionMap
                      case objectActionLookup of
                        Left err' -> handleAcquisitionError err'
                        Right (NotGettableF objectNotGettableF) -> objectNotGettableF
                        Right (CollectedF objectActionF) -> do
                          containerActionLookup <- lookupAcquisitionAction supportGID actionMap
                          case containerActionLookup of
                            Left err' -> handleAcquisitionError err'
                            Right (NotGettableF cannotGetFromF) -> cannotGetFromF
                            Right (LosesObjectF containerActionF) ->
                              finalize actionKey supportGID objectGID objectActionF containerActionF
                            Right _ -> handleAcquisitionError $ InvalidActionType $
                              "Container " <> (Data.Text.pack . show) supportGID <>
                              " does not have a LosesObjectF action."
                        Right _ -> handleAcquisitionError $ ObjectNotGettable $
                          "Object " <> (Data.Text.pack . show) objectGID <> " is not gettable."
          where
            findObjectByKey :: NounKey -> GameComputation Identity (Maybe (GID Object))
            findObjectByKey nounKey = do
              playerLocation <- getPlayerLocationM
              let objectSemanticMap = _objectSemanticMap playerLocation
              case Data.Map.Strict.lookup nounKey objectSemanticMap of
                Just objSet | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
                _ -> pure Nothing
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
                             -> GameComputation Identity (Either AcquisitionError AcquisitionActionF)
lookupAcquisitionAction objectGID actionMap = do
  actionMgmt <- _objectActionManagement <$> getObjectM objectGID
  case findAVKey get actionMgmt of
    Nothing -> pure $ Left $ ContainerMissingAction $ (Data.Text.pack . show) objectGID <> " does not have a 'get' action."
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> pure $ Left $ InvalidActionType $ "No acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> pure $ Right action
