-- Create new file: GameState/FieldEffectRegistry.hs
-- Mirror the pattern from GameState/EffectRegistry.hs

module GameState.FieldEffectRegistry where
import           Control.Monad.Identity     (Identity)
import           Control.Monad.State        (gets, modify')
import           Control.Monad.State.Strict (MonadState (get))
import qualified Data.Map.Strict
import qualified Data.Set                   as Set
import           Debug.Trace                (trace)
import           GameState                  (modifyLocationM, modifyObjectM,
                                             modifyPlayerM)
import           Model.GameState            (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                             ActionKey,
                                             FieldEffect (LocationTitleFieldEffect, ObjectDescriptionFieldEffect, ObjectShortNameFieldEffect, PlayerLocationFieldEffect),
                                             FieldEffectMap (FieldEffectMap),
                                             FieldEffectRegistry,
                                             GameComputation,
                                             GameState (_fieldEffectRegistry),
                                             Location (_title),
                                             Object (_description, _shortName),
                                             Player (_location))

-- Basic registry access functions
getGlobalFieldEffectRegistry :: GameComputation Identity FieldEffectRegistry
getGlobalFieldEffectRegistry = gets _fieldEffectRegistry

modifyGlobalFieldEffectRegistry :: (FieldEffectRegistry -> FieldEffectRegistry)
                                -> GameComputation Identity ()
modifyGlobalFieldEffectRegistry f = modify' $ \gs ->
  gs { _fieldEffectRegistry = f (_fieldEffectRegistry gs) }

lookupActionFieldEffectsInRegistry :: ActionKey -> GameComputation Identity (Maybe FieldEffectMap)
lookupActionFieldEffectsInRegistry actionKey = do
  Data.Map.Strict.lookup actionKey <$> getGlobalFieldEffectRegistry

-- Utility to register new field effects
registerFieldEffects :: ActionKey -> FieldEffectMap -> GameComputation Identity ()
registerFieldEffects actionKey effectMap =
  modifyGlobalFieldEffectRegistry $ \registry ->
    Data.Map.Strict.insert actionKey effectMap registry

-- Utility to modify existing field effects in registry
modifyRegisteredFieldEffects :: ActionKey
                             -> (FieldEffectMap -> FieldEffectMap)
                             -> GameComputation Identity ()
modifyRegisteredFieldEffects actionKey f =
  modifyGlobalFieldEffectRegistry $ \registry ->
    Data.Map.Strict.adjust f actionKey registry

processFieldEffectsFromRegistry :: ActionKey -> GameComputation Identity ()
processFieldEffectsFromRegistry actionKey = do
  trace ("DEBUG: processFieldEffectsFromRegistry called with: " ++ show actionKey) $ pure ()
  gameState <- get
  let registry = _fieldEffectRegistry gameState
  trace ("DEBUG: Full field effect registry keys: " ++ show (Data.Map.Strict.keys registry)) $ pure ()
  maybeFieldEffectMap <- lookupActionFieldEffectsInRegistry actionKey
  case maybeFieldEffectMap of
    Just fieldEffectMap -> do
      let FieldEffectMap fieldEffectMapContents = fieldEffectMap
      trace ("DEBUG: Found field effect map with keys: " ++ show (Data.Map.Strict.keys fieldEffectMapContents)) $ pure ()
      processAllFieldEffects fieldEffectMap
    Nothing -> do
      trace ("DEBUG: No field effects found for key: " ++ show actionKey) $ pure ()

processAllFieldEffects :: FieldEffectMap -> GameComputation Identity ()
processAllFieldEffects (FieldEffectMap fieldEffectMap) = do
  trace ("DEBUG: processAllFieldEffects called with field effect keys: " ++ show (Data.Map.Strict.keys fieldEffectMap)) $ pure ()
  mapM_ processFieldEffectEntry (Data.Map.Strict.toList fieldEffectMap)
  where
    processFieldEffectEntry :: (ActionEffectKey, Set.Set FieldEffect) -> GameComputation Identity ()
    processFieldEffectEntry (effectKey, fieldEffects) = do
      trace ("DEBUG: Processing field effect key: " ++ show effectKey ++ " with " ++ show (Set.size fieldEffects) ++ " field effects") $ pure ()
      mapM_ (processFieldEffect effectKey) (Set.toList fieldEffects)

processFieldEffect :: ActionEffectKey
                        -> FieldEffect
                        -> GameComputation Identity ()

-- OBJECT FIELD EFFECTS
processFieldEffect (ObjectKey oid) (ObjectShortNameFieldEffect newShortName _) = do
  trace ("DEBUG: Processing ObjectShortNameFieldEffect for object " ++ show oid ++ " with new short name: " ++ show newShortName) $ pure ()
  modifyObjectM oid $ \obj -> obj { _shortName = newShortName }

processFieldEffect (ObjectKey oid) (ObjectDescriptionFieldEffect newDescription _) = do
  trace ("DEBUG: Processing ObjectDescriptionFieldEffect for object " ++ show oid ++ " with new description: " ++ show newDescription) $ pure ()
  modifyObjectM oid $ \obj -> obj { _description = newDescription }

-- LOCATION FIELD EFFECTS
processFieldEffect (LocationKey lid) (LocationTitleFieldEffect newTitle _) = do
  trace ("DEBUG: Processing LocationTitleFieldEffect for location " ++ show lid ++ " with new title: " ++ show newTitle) $ pure ()
  modifyLocationM lid $ \loc -> loc { _title = newTitle }

-- PLAYER FIELD EFFECTS
processFieldEffect (PlayerKey playerKey) (PlayerLocationFieldEffect newLocationGID _) = do
  trace ("DEBUG: Processing PlayerLocationFieldEffect for player " ++ show playerKey ++ " with new location: " ++ show newLocationGID) $ pure ()
  modifyPlayerM $ \player -> player { _location = newLocationGID }

-- Catch-all for mismatched key/effect combinations
processFieldEffect effectKey fieldEffect = do
  trace ("WARNING: Mismatched field effect key/effect combination: " ++ show effectKey ++ " with " ++ show fieldEffect) $ pure ()

-- Utility to remove field effects from registry
unregisterFieldEffects :: ActionKey -> GameComputation Identity ()
unregisterFieldEffects actionKey =
  modifyGlobalFieldEffectRegistry $ \registry ->
    Data.Map.Strict.delete actionKey registry
