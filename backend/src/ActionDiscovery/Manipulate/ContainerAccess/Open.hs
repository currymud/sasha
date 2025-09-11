module ActionDiscovery.Manipulate.ContainerAccess.Open where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getPlayerLocationM, getPlayerM,
                                                modifyNarration,
                                                updateActionConsequence)
import           GameState.ActionManagement    (lookupContainerAccessVerbPhrase,
                                                processEffectsFromRegistry)
import           Model.Core                    (ActionMaps (_containerAccessActionMap),
                                                Config (_actionMaps),
                                                ContainerAccessActionF (CannotAccessF, InstrumentContainerAccessF, ObjectContainerAccessF, PlayerContainerAccessF),
                                                ContainerAccessResult (ContainerAccessResult),
                                                ActionEffectKey (ContainerAccessActionKey),
                                                GameComputation,
                                                Location (_objectSemanticMap),
                                                Player (_playerActions),
                                                SimpleAccessSearchStrategy)
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
finalizeContainerAccess :: ActionEffectKey
                        -> GameComputation Identity ContainerAccessResult
                        -> GameComputation Identity ()
finalizeContainerAccess effectActionKey objectActionF = do
 (ContainerAccessResult objectEffects objectFieldEffects) <- objectActionF
 let allEffects = effectActionKey:(objectEffects <> objectFieldEffects)
 mapM_ processEffectsFromRegistry allEffects
