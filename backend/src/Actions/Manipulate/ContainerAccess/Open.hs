module Actions.Manipulate.ContainerAccess.Open where
import           Control.Monad.Identity        (Identity)
import           Model.GameState               (GameComputation)
import           Model.Parser.Composites.Verbs (ContainerAccessVerbPhrase)

manageContainerAccessProcess :: ContainerAccessVerbPhrase -> GameComputation Identity ()
manageContainerAccessProcess cavp  = pure () {-

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
