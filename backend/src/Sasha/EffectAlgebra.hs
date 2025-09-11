{-# LANGUAGE TypeOperators #-}

module Sasha.EffectAlgebra where

import           Model.Core                (Effect, EffectActionKey)
import           Model.EDSL.SashaLambdaDSL (SashaLambdaDSL)
import           Sasha.HasEffect           (HasEffect, linkEffect)

-- | Composable effect that can chain
data EffectChain where
  Single :: (HasEffect a) => EffectActionKey -> a -> Effect -> EffectChain
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
effect :: (HasEffect a) => EffectActionKey -> a -> Effect -> EffectChain
effect = Single

-- | Build the composed effect computation
buildEffects :: EffectChain -> SashaLambdaDSL ()
buildEffects (Single key target eff) = linkEffect key target eff
buildEffects (Sequential left right) = buildEffects left >> buildEffects right
buildEffects (Parallel left right) = buildEffects left >> buildEffects right