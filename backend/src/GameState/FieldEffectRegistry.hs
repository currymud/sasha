-- Create new file: GameState/FieldEffectRegistry.hs
-- Mirror the pattern from GameState/EffectRegistry.hs

module GameState.FieldEffectRegistry where
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (gets, modify')
import qualified Data.Map.Strict
import           Model.GameState        (ActionKey, FieldEffectMap,
                                         FieldEffectRegistry, GameComputation,
                                         GameState (_fieldEffectRegistry))

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

-- Utility to remove field effects from registry
unregisterFieldEffects :: ActionKey -> GameComputation Identity ()
unregisterFieldEffects actionKey =
  modifyGlobalFieldEffectRegistry $ \registry ->
    Data.Map.Strict.delete actionKey registry
