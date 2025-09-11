module Sasha.EffectAlgebra where

import           Data.Kind           (Type)
import           Model.Core          (ActionEffectKey, Effect)
import           Model.EDSL.SashaDSL (SashaDSL)
import           Sasha.HasEffect     (HasEffect, linkEffect)

-- | Composable effect that can chain
type EffectChain :: Type
data EffectChain where
  Single :: (HasEffect a) => ActionEffectKey -> a -> Effect -> EffectChain
  Sequential :: EffectChain -> EffectChain -> EffectChain
  Parallel :: EffectChain -> EffectChain -> EffectChain

-- | Effect composition operators that return chainable values
infixr 1 `andThen`
infixr 2 `alongside`

-- | Sequential composition: chain effects in sequence
andThen :: EffectChain -> EffectChain -> EffectChain
andThen = Sequential

-- | Parallel composition: effects happen simultaneously
alongside :: EffectChain -> EffectChain -> EffectChain
alongside = Parallel

-- | Create a single effect
buildEffect :: (HasEffect a) => ActionEffectKey -> a -> Effect -> EffectChain
buildEffect = Single

-- | Build the composed effect computation
buildEffects :: EffectChain -> SashaDSL ()
buildEffects (Single key target eff) = linkEffect key target eff
buildEffects (Sequential left right) = buildEffects left >> buildEffects right
buildEffects (Parallel left right)   = buildEffects left >> buildEffects right
