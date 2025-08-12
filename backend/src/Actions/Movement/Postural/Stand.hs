module Actions.Movement.Postural.Stand  (managePosturalProcess) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationObjectIDsM,
                                                getPlayerM)
import           GameState.ActionManagement    (lookupPostural)
import           Model.GameState               (ActionEffectKey (LocationKey, PlayerKey),
                                                ActionEffectMap (ActionEffectMap),
                                                ActionKey (PosturalActionKey),
                                                ActionKeyMap (_unActionKeyMap),
                                                ActionMaps (_posturalActionMap),
                                                Config (_actionMaps),
                                                GameComputation,
                                                Player (_actionKeyMap, _location, _playerActions),
                                                PosturalActionF (PosturalActionF))
import           Model.Parser.Composites.Verbs (PosturalVerbPhrase)


managePosturalProcess :: PosturalVerbPhrase -> GameComputation Identity ()
managePosturalProcess posturalPhrase = do
 availableActions <- _playerActions <$> getPlayerM
 case lookupPostural posturalPhrase availableActions of
   Nothing -> error "Programmer Error: No postural action found for phrase: "
   Just actionGID -> do
     actionMap <- asks (_posturalActionMap . _actionMaps)
     case Data.Map.Strict.lookup actionGID actionMap of
       Nothing -> error $ "Programmer Error: No postural action found for GID: "
       Just (PosturalActionF actionFunc) -> do
         actionKeyMap <- _unActionKeyMap . _actionKeyMap <$> getPlayerM
         case Data.Map.Strict.lookup actionKey actionKeyMap of
           Nothing -> error $ "Programmer Error: No action key found for GID: "
           Just actionEffectMap@(ActionEffectMap effectMap) -> do
             lid <- _location <$> getPlayerM
             objectActionKeys <- getLocationObjectIDsM lid
             -- Add PlayerKey entries from the effect map to actionEffectKeys
             let locationKeys = Data.Set.insert (LocationKey lid) objectActionKeys
                 playerKeys = Data.Set.fromList [key | key@(PlayerKey _) <- Data.Map.Strict.keys effectMap]
                 allActionKeys = Data.Set.union locationKeys playerKeys
             actionFunc allActionKeys actionEffectMap
         where
           actionKey :: ActionKey
           actionKey = PosturalActionKey actionGID
