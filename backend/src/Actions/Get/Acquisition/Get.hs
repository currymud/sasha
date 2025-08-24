{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Actions.Get.Acquisition.Get (manageAcquisitionProcess) where

import           Build.BedPuzzle.Actions.Utils (AcquisitionError (SpatialValidationFailed),
                                                handleAcquisitionError)
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import           Control.Monad.State           (gets)
import qualified Data.Map.Strict
import           Data.Set                      (Set, elemAt, null, toList)
import qualified Data.Text
import           Debug.Trace                   (trace)
import           GameState                     (getPlayerLocationM, getPlayerM)
import           GameState.ActionManagement    (lookupAcquisitionVerbPhrase,
                                                processEffectsFromRegistry)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                ActionKey (AcquisitionalActionKey),
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                CoordinationResult (CoordinationResult),
                                                GameComputation,
                                                GameState (_world),
                                                Location (_objectSemanticMap),
                                                Object, Player (_playerActions),
                                                SearchStrategy,
                                                SpatialRelationship (ContainedIn, SupportedBy),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase)

-- we are removing processEffectsFromRegistry from here
manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupAcquisitionVerbPhrase avp availableActions of
    Nothing -> error "Programmer Error: No acquisition action found for phrase: "
    Just actionGID -> do
      trace ("DEBUG: Found player action GID: " ++ show actionGID) $ pure () -- ADD THIS
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No acquisition action found for GID: "
        Just (AcquisitionActionF actionFunc) -> do
          trace ("DEBUG: Executing AcquisitionActionF for actionGID: " ++ show actionGID) $ pure () -- ADD THIS
          let actionKey = AcquisitionalActionKey actionGID
          actionFunc actionKey actionMap locationSearchStrategy avp finalizeAcquisition
          trace ("DEBUG: About to call processEffectsFromRegistry with actionKey: " ++ show actionKey) $ pure () -- ADD THIS
          trace ("DEBUG: processEffectsFromRegistry completed for actionKey: " ++ show actionKey) $ pure () -- ADD THIS
        Just (LosesObjectF _actionFunc) -> do
          trace ("DEBUG: Found LosesObjectF for actionGID: " ++ show actionGID) $ pure () -- ADD THIS
          error "Drop actions not yet implemented" -- dropped objects need to process their own effects
        Just (NotGettableF actionF) -> actionF -- not gettables need to process their own effects
        Just (CollectedF _) ->
          trace ("DEBUG: Found CollectedF for actionGID: " ++ show actionGID) $  -- ADD THIS
          error "CollectedF should not be in player action map"

  {-
manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupAcquisitionVerbPhrase avp availableActions of
    Nothing -> error "Programmer Error: No acquisition action found for phrase: "
    Just actionGID -> do
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No acquisition action found for GID: "
        Just (AcquisitionActionF actionFunc) -> do
          let actionKey = AcquisitionalActionKey actionGID
          actionFunc locationSearchStrategy avp
          processEffectsFromRegistry actionKey
        Just (LosesObjectF _actionFunc) -> do
          let actionKey = AcquisitionalActionKey actionGID
          -- For drop actions, we'd need different logic here
          -- But for now, error since drop isn't implemented
          error "Drop actions not yet implemented"
          processEffectsFromRegistry actionKey
        Just (CollectedF _) -> error "CollectedF should not be in player action map"
        -}
-- | General case: Search current location's object semantic map
locationSearchStrategy :: SearchStrategy
locationSearchStrategy targetNounKey = do
  playerLocation <- getPlayerLocationM
  let objectSemanticMap = _objectSemanticMap playerLocation
  case Data.Map.Strict.lookup targetNounKey objectSemanticMap of
    Just objSet | not (Data.Set.null objSet) -> do
      let targetGID = Data.Set.elemAt 0 objSet
      -- Find what contains/supports this object
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
      case Data.Map.Strict.lookup targetGID spatialMap of
        Just relationships -> do
          let sources = getContainerSources relationships
          case sources of
            (sourceGID:_) -> pure $ Just (targetGID, sourceGID)
            []            -> pure Nothing  -- Object exists but has no container
        Nothing -> pure Nothing
    _ -> pure Nothing
  where
    getContainerSources :: Set SpatialRelationship -> [GID Object]
    getContainerSources relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]

finalizeAcquisition :: ActionKey
                        -> GID Object
                        -> GID Object
                        -> GameComputation Identity CoordinationResult
                        -> (GID Object -> GameComputation Identity CoordinationResult)
                        -> GameComputation Identity ()
finalizeAcquisition actionKey containerGID objectGID objectActionF containerActionF = do
  world <- gets _world
  let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
  case Data.Map.Strict.lookup objectGID spatialMap of
   Nothing -> handleAcquisitionError $ SpatialValidationFailed $ "No spatial relationships found for object " <> (Data.Text.pack . show) objectGID
   Just relationships -> do
     let isContainedInSource = any (\case
           ContainedIn oid -> oid == containerGID
           SupportedBy oid -> oid == containerGID
           _ -> False) (Data.Set.toList relationships)
     if not isContainedInSource
     then handleAcquisitionError $ SpatialValidationFailed $ "Object " <> (Data.Text.pack . show) objectGID <> " is not in or on container " <> (Data.Text.pack . show) containerGID
     else  do
       (CoordinationResult playerGetObjectF objectEffects) <- objectActionF
       (CoordinationResult containerRemoveObjectF containerEffects) <- containerActionF objectGID
       let allEffects = actionKey:(objectEffects <> containerEffects)
       mapM_ processEffectsFromRegistry allEffects >> containerRemoveObjectF >> playerGetObjectF
  {-
-- |  Search global perception map
perceptionSearchStrategy :: SearchStrategy
perceptionSearchStrategy targetNounKey = do
  world <- gets _world
  let perceptionMap = _perceptionMap world

  -- Convert NounKey to DirectionalStimulusNounPhrase for perception map lookup
  case nounKeyToDirectionalStimulus targetNounKey of
    Just dsnp -> case Data.Map.Strict.lookup dsnp perceptionMap of
      Just objSet | not (Data.Set.null objSet) -> do
        let targetGID = Data.Set.elemAt 0 objSet
        -- Find what contains/supports this object
        let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world
        case Data.Map.Strict.lookup targetGID spatialMap of
          Just relationships -> do
            let sources = getContainerSources relationships
            case sources of
              (sourceGID:_) -> pure $ Just (targetGID, sourceGID)
              []            -> pure Nothing
          Nothing -> pure Nothing
      _ -> pure Nothing
    Nothing -> pure Nothing
  where
    getContainerSources :: Set SpatialRelationship -> [GID Object]
    getContainerSources relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]

    -- Helper to convert NounKey to DirectionalStimulusNounPhrase
    nounKeyToDirectionalStimulus :: NounKey -> DirectionalStimulusNounPhrase
    nounKeyToDirectionalStimulus (DirectionalStimulusKey dsn) =
      DirectionalStimulusNounPhrase (SimpleNounPhrase dsn)
--    nounKeyToDirectionalStimulus (ObjectiveKey obj) =
-}
