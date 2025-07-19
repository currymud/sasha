module GameState (getActionF
                 , getObjectM
                 , getImplicitStimulusVerbProcessors
                 , getPlayerM
                 , getImplicitStimulusVerbProcessorGID
                 , getImplicitStimulusVerbProcessor
                 , liftGS) where
import           Control.Monad.Reader       (MonadReader (ask), asks)
import           Control.Monad.State        (get, gets)
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict
import           Data.Text                  (Text, pack)
import           Error                      (throwMaybeM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             Config (_actionMap, _sentenceProcessingMaps),
                                             GameComputation,
                                             GameState (_player, _world),
                                             GameStateExceptT,
                                             Object (_objectActionManagement),
                                             Player (_sentenceManagement),
                                             ProcessImplicitStimulusVerb,
                                             ProcessImplicitVerbMap,
                                             ProcessImplicitVerbMaps,
                                             ResolutionT (ResolutionT),
                                             SentenceProcessingMaps (_processImplicitVerbMap),
                                             World (_objectMap))
import           Model.GID                  (GID)
import           Model.Mappings             (_getGIDToDataMap)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Model.Parser.GCase         (VerbKey)
import           Relude                     (ToText (toText))

getActionF :: GID ActionF -> GameStateExceptT ActionF
getActionF vkey = do
  gs :: Config <- ask
  let amap = _getGIDToDataMap $ _actionMap gs
  throwMaybeM "Action not found in action map" $ Data.Map.Strict.lookup vkey amap

getImplicitStimulusVerbProcessorGID :: ImplicitStimulusVerb -> GameStateExceptT (GID ProcessImplicitStimulusVerb)
getImplicitStimulusVerbProcessorGID ivp = do
  sentenceManagement <- gets (_sentenceManagement . _player)
  throwMaybeM errMsg $ Data.Map.Strict.lookup ivp sentenceManagement
  where
    errMsg :: Text
    errMsg = "Implicit stimulus verb processor not found: " <> toText ivp

getImplicitStimulusVerbProcessor :: ImplicitStimulusVerb
                                      -> GameStateExceptT ProcessImplicitStimulusVerb
getImplicitStimulusVerbProcessor ivp = do
  pid <- getImplicitStimulusVerbProcessorGID ivp
  let errMsg = "Implicit stimulus verb processor not found for player: " <> toText pid
  processImplicitVerbMap <- getImplicitStimulusVerbProcessors ivp
  throwMaybeM errMsg $ Data.Map.Strict.lookup pid processImplicitVerbMap

getImplicitStimulusVerbProcessors :: ImplicitStimulusVerb
                                      -> GameStateExceptT ProcessImplicitVerbMap
getImplicitStimulusVerbProcessors ivp = do
  processImplicitVerbMap <- asks (_processImplicitVerbMap . _sentenceProcessingMaps)
  throwMaybeM errMsg $ Data.Map.Strict.lookup ivp processImplicitVerbMap
  where
    errMsg = "Implicit stimulus verb processor not found: " <> toText ivp
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

