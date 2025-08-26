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
                                                ActionMaps (_acquisitionActionMap),
                                                Config (_actionMaps),
                                                CoordinationResult (CoordinationResult),
                                                EffectActionKey (AcquisitionalActionKey),
                                                GameComputation,
                                                GameState (_world),
                                                Location (_objectSemanticMap),
                                                Object, Player (_playerActions),
                                                SearchStrategy,
                                                SpatialRelationship (ContainedIn, SupportedBy),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap))
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase (AcquisitionVerbPhrase, SimpleAcquisitionVerbPhrase))

-- we are removing processEffectsFromRegistry from here
manageAcquisitionProcess :: AcquisitionVerbPhrase -> GameComputation Identity ()
manageAcquisitionProcess avp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupAcquisitionVerbPhrase simpled availableActions of
    Nothing -> error "Programmer Error: No acquisition action found for phrase: "
    Just actionGID -> do
      actionMap <- asks (_acquisitionActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No acquisition action found for GID: "
        Just foundAction -> do
          case foundAction of
            (AcquisitionActionF actionFunc) -> do
               let actionKey = AcquisitionalActionKey actionGID
               actionFunc actionKey actionMap locationSearchStrategy avp finalizeAcquisition
            (LosesObjectF _actionFunc) -> do
              error "Drop actions not yet implemented"
            (NotGettableF actionF) -> do
              actionF
            (CollectedF _) ->
              error "CollectedF should not be in player action map"
  where
    simpled = simplifyAcquisitionVerbPhrase avp
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

simplifyAcquisitionVerbPhrase :: AcquisitionVerbPhrase -> AcquisitionVerbPhrase
simplifyAcquisitionVerbPhrase unchanged@(SimpleAcquisitionVerbPhrase _ _) = unchanged
simplifyAcquisitionVerbPhrase (AcquisitionVerbPhrase verb objectPhrase _ _) = SimpleAcquisitionVerbPhrase verb objectPhrase

finalizeAcquisition :: EffectActionKey
                        -> GID Object
                        -> GID Object
                        -> GameComputation Identity CoordinationResult
                        -> (GID Object -> GameComputation Identity CoordinationResult)
                        -> GameComputation Identity ()
finalizeAcquisition effectActionKey containerGID objectGID objectActionF containerActionF = do
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
       (CoordinationResult playerGetObjectF objectEffects objectFieldEffects) <- objectActionF
       (CoordinationResult containerRemoveObjectF containerEffects containerFieldEffects) <- containerActionF objectGID
       let allEffects = effectActionKey:(objectEffects <> containerEffects <> objectFieldEffects <> containerFieldEffects)
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
