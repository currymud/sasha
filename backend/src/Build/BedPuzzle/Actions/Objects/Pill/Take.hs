module Build.BedPuzzle.Actions.Objects.Pill.Take (alreadyTookPillF, pillTooFarF, takePillF, takePillDeniedF) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.State           (modify')
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Set                      (Set)
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (modifyNarration,
                                                modifyObjectActionManagementM,
                                                modifySpatialRelationshipsForObjectM)
import           Model.GameState               (ActionEffectKey (ObjectKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionManagement (DSAManagementKey, NPManagementKey, PPManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ConsumptionActionF (ConsumptionActionF),
                                                Effect (DirectionalStimulusEffect, NegativePosturalEffect, PositivePosturalEffect),
                                                GameComputation,
                                                GameState (_player), Object,
                                                Player (_playerActions),
                                                PlayerKey (PlayerKeyObject),
                                                SpatialRelationship (ContainedIn, SupportedBy),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ConsumptionVerbPhrase)

takePillDeniedF :: ConsumptionActionF
takePillDeniedF = ConsumptionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't take the pill right now."


alreadyTookPillF :: ConsumptionActionF
alreadyTookPillF = ConsumptionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You already took the pill."

pillTooFarF :: ConsumptionActionF
pillTooFarF = ConsumptionActionF (const (const (const denied)))
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You grab at it but it's hard to get to. try grabbing the robe first."

takePillF :: ConsumptionActionF
takePillF = ConsumptionActionF takeIt
  where
    takeIt :: Set ActionEffectKey -> ActionEffectMap -> GameComputation Identity ()
    takeIt actionEffectKeys (ActionEffectMap actionEffectMap) = do
      -- Find the pill object from the action effect keys
      let pillObjectKeys = [oid | ObjectKey oid <- Data.Set.toList actionEffectKeys]

      case pillObjectKeys of
        (pillOID:_) -> do
          -- Remove the pill from whatever is containing/supporting it
          modifySpatialRelationshipsForObjectM pillOID $ \relationships ->
            Data.Set.filter (not . isLocationRelationship) relationships

          -- Fire the consumption consequence
          modifyNarration $ updateActionConsequence "You take the pill and swallow it."

          -- Process effects for all action effect keys
          mapM_ (processEffectKeyEntry actionEffectMap) (Data.Set.toList actionEffectKeys)
        [] -> modifyNarration $ updateActionConsequence "No pill found to consume."

    processEffectKeyEntry :: Map ActionEffectKey (Set Effect)
                          -> ActionEffectKey
                          -> GameComputation Identity ()
    processEffectKeyEntry effectMap actionEffectKey = do
      case Data.Map.Strict.lookup actionEffectKey effectMap of
        Nothing -> pure ()
        Just effects -> mapM_ (processEffect actionEffectKey) (Data.Set.toList effects)

    processEffect :: ActionEffectKey -> Effect -> GameComputation Identity ()
    processEffect (PlayerKey (PlayerKeyObject oid)) (PositivePosturalEffect verb newActionGID) = do
      -- Update the player's positive postural actions
      modify' $ \gs ->
        let player = gs._player
            ActionManagementFunctions playerActionSet = _playerActions player
            -- Remove any existing positive postural action for this verb
            filteredActions = Data.Set.filter (\case PPManagementKey v _ -> v /= verb; _ -> True) playerActionSet
            -- Add the new action
            updatedActions = Data.Set.insert (PPManagementKey verb newActionGID) filteredActions
            updatedPlayerActions = ActionManagementFunctions updatedActions
            updatedPlayer = player { _playerActions = updatedPlayerActions }
        in gs { _player = updatedPlayer }

    processEffect (PlayerKey (PlayerKeyObject oid)) (NegativePosturalEffect verb newActionGID) = do
      -- Update the player's negative postural actions
      modify' $ \gs ->
        let player = gs._player
            ActionManagementFunctions playerActionSet = _playerActions player
            -- Remove any existing negative postural action for this verb
            filteredActions = Data.Set.filter (\case NPManagementKey v _ -> v /= verb; _ -> True) playerActionSet
            -- Add the new action
            updatedActions = Data.Set.insert (NPManagementKey verb newActionGID) filteredActions
            updatedPlayerActions = ActionManagementFunctions updatedActions
            updatedPlayer = player { _playerActions = updatedPlayerActions }
        in gs { _player = updatedPlayer }

    processEffect (ObjectKey oid) (DirectionalStimulusEffect verb newActionGID) = do
      -- Update the object's directional stimulus action
      modifyObjectActionManagementM oid $ \actionMgmt ->
        let ActionManagementFunctions actionSet = actionMgmt
            filteredActions = Data.Set.filter (\case DSAManagementKey v _ -> v /= verb; _ -> True) actionSet
            updatedActions = Data.Set.insert (DSAManagementKey verb newActionGID) filteredActions
        in ActionManagementFunctions updatedActions

    processEffect _ _ = pure () -- Handle other effect types as needed

    isLocationRelationship :: SpatialRelationship -> Bool
    isLocationRelationship (ContainedIn _) = True
    isLocationRelationship (SupportedBy _) = True
    isLocationRelationship _               = False
