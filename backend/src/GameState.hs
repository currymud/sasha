module GameState ( clearNarration
                 , getObjectM
                 , getPlayerM
                 , getPlayerActionsM
                 , getPlayerLocationM
                 , modifyNarration) where
import           Control.Monad.Reader       (MonadReader (ask), asks)
import           Control.Monad.State        (gets, modify')
import qualified Data.Map.Strict
import           Data.Text                  (Text, pack)
import           Error                      (throwMaybeM)
import           Model.GameState            (Config (_actionMaps, _sentenceProcessingMaps),
                                             GameComputation,
                                             GameState (_narration, _player, _world),
                                             ImplicitStimulusActionF, Location,
                                             Narration (Narration),
                                             Object (_objectActionManagement),
                                             Player (_location, _playerActions),
                                             PlayerActions,
                                             PlayerSentenceProcessingMaps (_playerProcessImplicitVerbMap),
                                             ProcessImplicitStimulusVerb,
                                             ProcessImplicitVerbMap,
                                             SentenceProcessingMaps (_processImplicitVerbMap),
                                             World (_locationMap, _objectMap))
import           Model.GID                  (GID)
import           Model.Mappings             (_getGIDToDataMap)
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Relude                     (Identity, ToText (toText))

getPlayerActionsM :: GameComputation Identity  PlayerActions
getPlayerActionsM =  _playerActions <$> getPlayerM

getPlayerM :: GameComputation Identity Player
getPlayerM = gets _player

getObjectM :: GID Object -> GameComputation Identity Object
getObjectM oid = do
  objMap <- gets (_getGIDToDataMap . _objectMap . _world)
  throwMaybeM ("Object not found in object map" <> pack (show oid)) $ Data.Map.Strict.lookup oid objMap

getPlayerLocationM :: GameComputation Identity Location
getPlayerLocationM = do
  location <- _location <$> getPlayerM
  locationMap <- gets (_getGIDToDataMap . _locationMap . _world)
  throwMaybeM "Player location not found" $ Data.Map.Strict.lookup location locationMap

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
