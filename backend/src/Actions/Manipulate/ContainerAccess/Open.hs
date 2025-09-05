module Actions.Manipulate.ContainerAccess.Open where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getPlayerM, modifyNarration)
import           GameState.ActionManagement    (lookupContainerAccessVerbPhrase,
                                                processEffectsFromRegistry)
import           Location                      (getPlayerLocationM)
import           Model.GameState               (ActionMaps (_containerAccessActionMap),
                                                Config (_actionMaps),
                                                ContainerAccessActionF (CannotAccessF, InstrumentContainerAccessF, ObjectContainerAccessF, PlayerContainerAccessF),
                                                ContainerAccessResult (ContainerAccessResult),
                                                EffectActionKey (ContainerAccessActionKey),
                                                GameComputation,
                                                Location (_objectSemanticMap),
                                                Player (_playerActions),
                                                SimpleAccessSearchStrategy,
                                                updateActionConsequence)
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
        Just (InstrumentContainerAccessF _) -> error "InstrumentContainerAccessF is not a player constructor"
        Just (CannotAccessF actionF) -> actionF  -- Execute the failure action
        Just (ObjectContainerAccessF _) -> error "ObjectContainerAccessF is not a player constructor"
        Just (PlayerContainerAccessF actionF) -> do
          let effectActionKey = ContainerAccessActionKey actionGID
          actionF effectActionKey objectSearchStrategy actionMap cavp finalizeContainerAccess

-- | ToDo Clarification system. object identification assumes only one object in location matches nounkey.
objectSearchStrategy :: SimpleAccessSearchStrategy
objectSearchStrategy nounkey = do
  objectSemanticMap <- _objectSemanticMap <$> getPlayerLocationM
  case Data.Map.Strict.lookup nounkey objectSemanticMap of
    Just objSet
      | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
    _ -> pure Nothing
finalizeContainerAccess :: EffectActionKey
                        -> GameComputation Identity ContainerAccessResult
                        -> GameComputation Identity ()
finalizeContainerAccess effectActionKey objectActionF = do
 (ContainerAccessResult objectEffects objectFieldEffects) <- objectActionF
 let allEffects = effectActionKey:(objectEffects <> objectFieldEffects)
 mapM_ processEffectsFromRegistry allEffects
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
