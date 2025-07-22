module GameState ( clearNarration
                 , getActionF
                 , getObjectM
                 , getImplicitStimulusVerbProcessors
                 , getPlayerM
                 , getImplicitStimulusVerbProcessorGID
                 , getImplicitStimulusVerbProcessor
                 , modifyNarration) where
import           Control.Monad.Reader       (MonadReader (ask), asks)
import           Control.Monad.State        (gets, modify')
import qualified Data.Map.Strict
import           Data.Text                  (Text, pack)
import           Error                      (throwMaybeM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             Config (_actionMap, _sentenceProcessingMaps),
                                             GameComputation,
                                             GameState (_narration, _player, _world),
                                             Narration (Narration),
                                             Object (_objectActionManagement),
                                             Player (_sentenceManagement),
                                             ProcessImplicitStimulusVerb,
                                             ProcessImplicitVerbMap,
                                             SentenceProcessingMaps (_processImplicitVerbMap),
                                             World (_objectMap))
import           Model.GID                  (GID)
import           Model.Mappings             (_getGIDToDataMap)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Relude                     (Identity, ToText (toText))

getActionF :: GID ActionF -> GameComputation Identity ActionF
getActionF vkey = do
  gs :: Config <- ask
  let amap = _getGIDToDataMap $ _actionMap gs
  throwMaybeM "Action not found in action map" $ Data.Map.Strict.lookup vkey amap

getImplicitStimulusVerbProcessorGID :: ImplicitStimulusVerb
                                         -> GameComputation Identity (GID ProcessImplicitStimulusVerb)
getImplicitStimulusVerbProcessorGID ivp = do
  sentenceManagement <- gets (_sentenceManagement . _player)
  throwMaybeM errMsg $ Data.Map.Strict.lookup ivp sentenceManagement
  where
    errMsg :: Text
    errMsg = "Implicit stimulus verb processor not found: " <> toText ivp

getImplicitStimulusVerbProcessor :: ImplicitStimulusVerb
                                      -> GameComputation Identity ProcessImplicitStimulusVerb
getImplicitStimulusVerbProcessor ivp = do
  pid <- getImplicitStimulusVerbProcessorGID ivp
  let errMsg = "Implicit stimulus verb processor not found for player: " <> toText pid
  processImplicitVerbMap <- getImplicitStimulusVerbProcessors ivp
  throwMaybeM errMsg $ Data.Map.Strict.lookup pid processImplicitVerbMap

getImplicitStimulusVerbProcessors :: ImplicitStimulusVerb
                                      -> GameComputation Identity ProcessImplicitVerbMap
getImplicitStimulusVerbProcessors ivp = do
  processImplicitVerbMap <- asks (_processImplicitVerbMap . _sentenceProcessingMaps)
  throwMaybeM errMsg $ Data.Map.Strict.lookup ivp processImplicitVerbMap
  where
    errMsg = "Implicit stimulus verb processor not found: " <> toText ivp

getPlayerM :: GameComputation Identity Player
getPlayerM = gets _player

getObjectM :: GID Object -> GameComputation Identity Object
getObjectM oid = do
  objMap <- gets (_getGIDToDataMap . _objectMap . _world)
  throwMaybeM ("Object not found in object map" <> pack (show oid)) $ Data.Map.Strict.lookup oid objMap

modifyNarration :: (Narration -> Narration)
                     -> GameComputation Identity ()
modifyNarration narrationF = do
  current_narration <- gets _narration
  let updatedNarrative = narrationF current_narration
  modify' (\gs -> gs{ _narration = updatedNarrative })

clearNarration :: GameComputation Identity ()
clearNarration = modifyNarration (const emptyNarration)
  where
    emptyNarration :: Narration
    emptyNarration = Narration mempty mempty
