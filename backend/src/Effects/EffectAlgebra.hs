module Effects.EffectAlgebra where

import           Data.Kind                 (Type)
import           Model.Core                (Effect, ActionEffectKey)
import           Model.EDSL.SashaLambdaDSL (SashaLambdaDSL)
import           Effects.HasEffect           (HasEffect, linkEffect)

-- | Composable effect that can chain
type EffectChain :: Type
data EffectChain where
  Single :: (HasEffect a) => ActionEffectKey -> a -> Effect -> EffectChain
  Composed :: EffectChain -> EffectChain -> EffectChain

-- | Effect composition operator
infixr 2 `alongside`

-- | Compose effects together
alongside :: EffectChain -> EffectChain -> EffectChain
alongside = Composed

-- | Create a single effect
buildEffect :: (HasEffect a) => ActionEffectKey -> a -> Effect -> EffectChain
buildEffect = Single

-- | Build the composed effect computation
buildEffects :: EffectChain -> SashaLambdaDSL ()
buildEffects (Single key target eff) = linkEffect key target eff
buildEffects (Composed left right)   = buildEffects left >> buildEffects right
