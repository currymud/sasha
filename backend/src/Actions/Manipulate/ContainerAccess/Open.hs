module Actions.Manipulate.ContainerAccess.Open where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import qualified Data.Map.Strict
import           GameState                     (getPlayerM, modifyNarration)
import           GameState.ActionManagement    (lookupContainerAccessVerbPhrase)
import           Model.GameState               (ActionMaps (_containerAccessActionMap),
                                                Config (_actionMaps),
                                                ContainerAccessActionF (CannotAccessF, ObjectContainerAccessF, PlayerContainerAccessF),
                                                GameComputation,
                                                Player (_playerActions),
                                                SearchStrategy,
                                                SpatialRelationship,
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Composites.Verbs (ContainerAccessVerbPhrase)

manageContainerAccessProcess :: ContainerAccessVerbPhrase -> GameComputation Identity ()
manageContainerAccessProcess cavp  = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupContainerAccessVerbPhrase cavp availableActions of
    Nothing -> modifyNarration $ updateActionConsequence "This container cannot be opened."
    Just actionGID -> do
      actionMap <- asks (_containerAccessActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error $ "Programmer Error: No container access action found for GID: " ++ show actionGID
        Just (CannotAccessF actionF) -> actionF  -- Execute the failure action
        Just (ObjectContainerAccessF actionF) -> error "ObjectContainerAccessF is not a player constructor"
        Just (PlayerContainerAccessF actionFunc) ->
          -- PlayerContainerAccessF would contain the full player-level action
          -- This is where the actual player open action would be stored
          error "PlayerContainerAccessF not yet implemented - this will be Build.BedPuzzle.Actions.Player.Open"

{-

do
  availableActions <- _playerActions <$> getPlayerM
  case lookupSomaticAccess sav availableActions of
    Nothing -> error "Programmer Error: No somatic access action found for verb: "
    Just actionGID -> do
      actionMap <- asks (_containerAccessActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No somatic access action found for GID: "
        Just (CannotAccessF actionF) -> pure () -- placeholder
        Just (ObjectContainerAccessF actionF) -> pure () -- placeholder
        Just (PlayerContainerAccessF actionFunc) -> do
          let actionKey = ContainerAccessActionKey actionGID
          maybeEffectMap <- lookupActionEffectsInRegistry actionKey
          case maybeEffectMap of
            Nothing -> error "Programmer Error: No effects registered for somatic access action"
            Just (ActionEffectMap effectMap) -> do
              lid <- _location <$> getPlayerM
              objectActionKeys <- getLocationObjectIDsM lid

              -- Get SystemEffectKeys from GameState
              systemEffectKeysRegistry <- gets _actionSystemEffectKeys
              let systemEffectKeysForAction = Data.Map.Strict.findWithDefault [] actionKey systemEffectKeysRegistry

              -- Get SystemEffectRegistry from GameState
              systemEffectRegistry <- gets _systemEffectRegistry

              let playerKeys = Data.Set.fromList [key | key@(PlayerKey _) <- Data.Map.Strict.keys effectMap]
                  allActionKeys = Data.Set.unions [
                    Data.Set.singleton (LocationKey lid),
                    objectActionKeys,
                    playerKeys
                    ]

              actionFunc allActionKeys systemEffectKeysForAction (ActionEffectMap effectMap) systemEffectRegistry
              processEffectsFromRegistry actionKey
              -}
