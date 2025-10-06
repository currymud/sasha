module ConstraintRefinement.Actions.Player.Get (getF,getDeniedF) where
import           Control.Monad.Error.Class                        (throwError)
import           Control.Monad.Identity                           (Identity)
import           Control.Monad.State                              (gets)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                                        (pack)
import           GameState                                        (getObjectM,
                                                                   getPlayerLocationM,
                                                                   parseAcquisitionPhrase)
import           GameState.ActionManagement                       (findAVKey,
                                                                   processEffectsFromRegistry)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Actions.Results                            (AcquisitionRes (Complete, Simple),
                                                                   CompleteAcquisitionRes (CompleteAcquisitionRes, _caObjectKey, _caObjectPhrase, _caSupportKey, _caSupportPhrase),
                                                                   SimpleAcquisitionRes (SimpleAcquisitionRes, _saObjectKey))
import           Model.Core                                       (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                                   AcquisitionVerbActionMap,
                                                                   ActionEffectKey (AcquisitionalActionKey),
                                                                   ActionManagementFunctions,
                                                                   FinalizeAcquisitionF,
                                                                   GameComputation,
                                                                   GameState (_world),
                                                                   Location (_objectSemanticMap),
                                                                   Object (_objectActionManagement, _shortName),
                                                                   SearchStrategy,
                                                                   SpatialRelationship (ContainedIn, SupportedBy),
                                                                   SpatialRelationshipMap (SpatialRelationshipMap),
                                                                   World (_spatialRelationshipMap))
import           Model.GID                                        (GID)
import           Model.Parser.Composites.Verbs                    (AcquisitionVerbPhrase)
import           Model.Parser.GCase                               (NounKey)

getDeniedF :: GID Object -> AcquisitionActionF
getDeniedF oid = NotGettableF denied
  where
    denied :: (ActionManagementFunctions -> Maybe (GID AcquisitionActionF))
                  -> GameComputation Identity ActionEffectKey
    denied lookupActionF = do
      actionManagement <- _objectActionManagement <$> getObjectM oid
      case lookupActionF actionManagement of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show oid)
        Just actionGID ->
          let actionKey = AcquisitionalActionKey actionGID
          in pure actionKey

getF :: AcquisitionActionF
getF = AcquisitionActionF getit
  where
    getit :: ActionEffectKey
               -> (ActionManagementFunctions -> Maybe (GID AcquisitionActionF))
               -> AcquisitionVerbActionMap
               -> SearchStrategy
               -> AcquisitionVerbPhrase
               -> FinalizeAcquisitionF
               -> GameComputation Identity ()
    getit actionEffectKey lookupActionF actionMap searchStrategy avp finalize = do
      case ares of
        Simple (SimpleAcquisitionRes {..}) -> do
          (objectGID, containerGID) <- validateObjectSearch searchStrategy _saObjectKey
          objectAction <- lookupAcquisitionAction objectGID actionMap
          case objectAction of
            (NotGettableF objectNotGettableF) -> objectNotGettableF lookupActionF >>= processEffectsFromRegistry
            (CollectedF objectActionF) -> do
              containerAction <- lookupAcquisitionAction containerGID actionMap
              case containerAction of
                (NotGettableF cannotGetFromF) -> cannotGetFromF lookupActionF >>= processEffectsFromRegistry
                (LosesObjectF containerActionF) -> finalize actionEffectKey containerGID objectGID objectActionF containerActionF
                _ -> throwError $ "Container " <> (Data.Text.pack . show) containerGID <> " does not have a LosesObjectF action."
            (LosesObjectF _) -> error (("Programmer Error: Object " <> show objectGID) <> " has a LosesObjectF action, which is invalid for get actions.")
            (AcquisitionActionF _) -> error (("Programmer Error: Object " <> show objectGID) <> " has an AcquisitionActionF action, which is invalid for get actions.")
        Complete (CompleteAcquisitionRes {..}) -> do
          -- Find both objects directly
          objectResult <- findObjectByKey _caObjectKey
          supportResult <- findObjectByKey _caSupportKey

          case (objectResult, supportResult) of
            (Nothing, _) -> throwError "You don't see that object here."
            (Just oid, Nothing) -> error ("programer error: support object not found for" <> show oid)
            (Just objectGID, Just supportGID) -> do
              -- Validate the object is actually on/in the support
              world <- gets _world
              let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
              case Data.Map.Strict.lookup objectGID spatialMap of
                Nothing -> error ("programmer error: " <> show objectGID <> " has no spatial relationships")
                Just relationships -> do
                  let isOnSupport = any (\case
                        SupportedBy sid -> sid == supportGID
                        ContainedIn cid -> cid == supportGID
                        _ -> False) (Data.Set.toList relationships)
                  if not isOnSupport
                    then do
                      objName <- _shortName <$> getObjectM objectGID
                      supportName <- _shortName <$> getObjectM supportGID
                      throwError $
                        "The " <> objName <>
                        " is not on the " <> supportName <> "."
                    else do
                      -- Now proceed with the standard lookups
                      objectAction <- lookupAcquisitionAction objectGID actionMap
                      case objectAction of
                        (NotGettableF objectNotGettableF) -> objectNotGettableF lookupActionF >>= processEffectsFromRegistry
                        (CollectedF objectActionF) -> do
                          containerAction <- lookupAcquisitionAction supportGID actionMap
                          case containerAction of
                            (NotGettableF cannotGetFromF) -> cannotGetFromF lookupActionF >>= processEffectsFromRegistry
                            (LosesObjectF containerActionF) ->
                              finalize actionEffectKey supportGID objectGID objectActionF containerActionF
                            _ -> error $
                              "Programmer error. Container " <> show supportGID <>
                              " does not have a LosesObjectF action."
                        _ -> throwError $
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

validateObjectSearch :: SearchStrategy
                          -> NounKey
                          -> GameComputation Identity (GID Object, GID Object)
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing                        -> throwError "You don't see that here."
    Just (objectGID, containerGID) -> pure (objectGID, containerGID)

lookupAcquisitionAction :: GID Object
                             -> AcquisitionVerbActionMap
                             -> GameComputation Identity AcquisitionActionF
lookupAcquisitionAction objectGID actionMap = do
  actionMgmt <- _objectActionManagement <$> getObjectM objectGID
  case findAVKey get actionMgmt of
    Nothing -> throwError $ (Data.Text.pack . show) objectGID <> " does not have a 'get' action."
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> throwError $ "No acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> pure action
