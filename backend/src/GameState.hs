module GameState (getActionF,  getObjectM, getPlayerM, liftGS) where
import           Control.Monad.Reader       (MonadReader (ask), asks)
import           Control.Monad.State        (get, gets)
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict
import           Data.Text                  (Text, pack)
import           Error                      (throwMaybeM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             Config (_actionMap),
                                             GameState (_player, _world),
                                             GameStateExceptT,
                                             Object (_objectActionManagement),
                                             Player (_sentenceManagement),
                                             ProcessImplicitVerbMap,
                                             ResolutionT (ResolutionT),
                                             SentenceProcessingMaps (_processImplicitVerbMap),
                                             World (_objectMap))
import           Model.GID                  (GID)
import           Model.Mappings             (_getGIDToDataMap)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Model.Parser.GCase         (VerbKey)

getActionF :: GID ActionF -> GameStateExceptT ActionF
getActionF vkey = do
  gs :: Config <- ask
  let amap = _getGIDToDataMap $ _actionMap gs
  throwMaybeM "Action not found in action map" $ Data.Map.Strict.lookup vkey amap
    {-
getPlayerActionF :: VerbKey -> GameStateExceptT (Either Text ActionF)
getPlayerActionF vkey = do
  oid <- gets ( _object . _player)
  pamap <- getObjectM oid <&> _objectActionManagement
  case Data.Map.Strict.lookup vkey pamap of
    Just actionF -> Right <$> getActionF actionF
    Nothing      -> pure $ Left $ "Action not found in player action map: " <> pack (show vkey)
-}
      {-
getPlayerImplicitStimulusActionF :: VerbKey -> GameStateExceptT ActionF
getPlayerImplicitStimulusActionF verbKey = do
  actionF <- getPlayerActionF verbKey
  case actionF of
    ImplicitStimulusAction action -> getActionF action
    -}

getPlayerM :: GameStateExceptT Player
getPlayerM = gets _player

  {-
getPlayerImplicitStimulusActionF :: ImplicitStimulusVerb
                                      -> GameStateExceptT (Either Text (Map (GID ProcessImplicitStimulusVerb) ProcessImplicitStimulusVerb)
getPlayerImplicitStimulusActionF verb = do
 ivpMap <- _processImplicitVerbMap <$> _sentenceManagement <$> getPlayerM
 case Data.Map.Strict.lookup verb ivpMap of
  Just ivp -> pure $ Right ivp
  Nothing      -> pure $ Left $ "Implicit stimulus action not found for verb: " <> pack (show verb)
-}
getObjectM :: GID Object -> GameStateExceptT Object
getObjectM oid = do
  objMap <- gets (_getGIDToDataMap . _objectMap . _world)
  throwMaybeM ("Object not found in object map" <> pack (show oid)) $ Data.Map.Strict.lookup oid objMap

liftGS :: GameStateExceptT a -> ResolutionT a
liftGS = ResolutionT

