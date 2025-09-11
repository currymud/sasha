module GameState.EffectRegistry where
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (gets, modify')
import qualified Data.Map.Strict
import           Model.Core             (ActionEffectMap, ActionEffectKey,
                                         EffectRegistry, GameComputation,
                                         GameState (_effectRegistry))

getGlobalEffectRegistry :: GameComputation Identity EffectRegistry
getGlobalEffectRegistry = gets _effectRegistry

modifyGlobalEffectRegistry :: (EffectRegistry -> EffectRegistry)
                           -> GameComputation Identity ()
modifyGlobalEffectRegistry f = modify' $ \gs ->
  gs { _effectRegistry = f (_effectRegistry gs) }

lookupActionEffectsInRegistry :: ActionEffectKey -> GameComputation Identity (Maybe ActionEffectMap)
lookupActionEffectsInRegistry actionKey = do
  Data.Map.Strict.lookup actionKey <$> getGlobalEffectRegistry

-- Utility to register new effects
registerEffects :: ActionEffectKey -> ActionEffectMap -> GameComputation Identity ()
registerEffects actionKey effectMap =
  modifyGlobalEffectRegistry $ \registry ->
    Data.Map.Strict.insert actionKey effectMap registry

-- Utility to modify existing effects in registry
modifyRegisteredEffects :: ActionEffectKey
                        -> (ActionEffectMap -> ActionEffectMap)
                        -> GameComputation Identity ()
modifyRegisteredEffects actionKey f =
  modifyGlobalEffectRegistry $ \registry ->
    Data.Map.Strict.adjust f actionKey registry

-- Utility to remove effects from registry
unregisterEffects :: ActionEffectKey -> GameComputation Identity ()
unregisterEffects actionKey =
  modifyGlobalEffectRegistry $ \registry ->
    Data.Map.Strict.delete actionKey registry
