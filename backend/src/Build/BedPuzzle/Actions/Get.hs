{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (get,getDenied) where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (gets, modify')
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
                                                GameState (_player, _world),
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_objectActionManagement),
                                                Player (_playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap),
                                                updateActionConsequence)
import           Model.GID                     (GID)
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
    getit loc actionEffectMap avp = do
      let (objectPhrase, nounKey) = parseAcquisitionPhrase avp

      -- Find the object in location semantic map
      case Data.Map.Strict.lookup nounKey loc._objectSemanticMap of
        Just objSet | not (Data.Set.null objSet) -> do
          let targetObjectGID = Data.Set.elemAt 0 objSet

          -- Find what contains/supports this object using spatial relationships
          world <- gets _world
          let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

          case Data.Map.Strict.lookup targetObjectGID spatialMap of
            Just relationships -> do
              let containerGIDs = getContainerGIDs relationships

              case containerGIDs of
                (containerGID:_) -> do
                  -- Call container's action to handle spatial relationship changes
                  callContainerAction containerGID avp targetObjectGID

                  -- Call object's action to handle inventory GID tracking
                  callObjectAction targetObjectGID loc avp

                [] -> do
                  -- Object not in a container, just call object action
                  callObjectAction targetObjectGID loc avp

            Nothing -> do
              -- No spatial relationships, just call object action
              callObjectAction targetObjectGID loc avp

        _ -> modifyNarration $ updateActionConsequence "You don't see that here."

    -- Extract container/supporter GIDs from spatial relationships
    getContainerGIDs :: Set SpatialRelationship -> [GID Object]
    getContainerGIDs relationships =
      [containerGID | ContainedIn containerGID <- Data.Set.toList relationships] ++
      [supporterGID | SupportedBy supporterGID <- Data.Set.toList relationships]

    -- Handle spatial relationship changes: remove from container and set to Inventory
    callContainerAction :: GID Object -> AcquisitionVerbPhrase -> GID Object -> GameComputation Identity ()
    callContainerAction containerGID avp targetObjectGID = do
      -- Remove object from container's Contains/Supports relationship
      modifySpatialRelationshipsForObjectM containerGID $ \relationships ->
        let updatedRelationships = Data.Set.map removeFromContainerRelationship relationships
            -- Remove empty Contains/Supports sets
            cleanedRelationships = Data.Set.filter (not . isEmpty) updatedRelationships
        in cleanedRelationships

      -- Set object's spatial relationship to Inventory
      modifySpatialRelationshipsForObjectM targetObjectGID $ \relationships ->
        -- Remove old containment/support relationships and add Inventory
        let cleanedRelationships = Data.Set.filter (not . isContainmentRelationship) relationships
        in Data.Set.insert Inventory cleanedRelationships

      where
        removeFromContainerRelationship :: SpatialRelationship -> SpatialRelationship
        removeFromContainerRelationship (Contains objSet) =
          Contains (Data.Set.delete targetObjectGID objSet)
        removeFromContainerRelationship (Supports objSet) =
          Supports (Data.Set.delete targetObjectGID objSet)
        removeFromContainerRelationship other = other

        isEmpty :: SpatialRelationship -> Bool
        isEmpty (Contains objSet) = Data.Set.null objSet
        isEmpty (Supports objSet) = Data.Set.null objSet
        isEmpty _                 = False

        isContainmentRelationship :: SpatialRelationship -> Bool
        isContainmentRelationship (ContainedIn _) = True
        isContainmentRelationship (SupportedBy _) = True
        isContainmentRelationship Inventory       = True  -- Remove existing Inventory too
        isContainmentRelationship _               = False

    -- Call the object's action to handle inventory tracking
    callObjectAction :: GID Object -> Location -> AcquisitionVerbPhrase -> GameComputation Identity ()
    callObjectAction targetObjectGID loc avp = do
      -- Object's only job: add its GID to player's inventory
      addToInventoryM targetObjectGID
