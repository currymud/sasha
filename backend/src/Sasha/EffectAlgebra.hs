{-# LANGUAGE TypeOperators #-}

module Sasha.EffectAlgebra where

import           Model.Core                (Effect, EffectActionKey)
import           Model.EDSL.SashaLambdaDSL (SashaLambdaDSL)
import           Sasha.HasEffect           (HasEffect, linkEffect)

-- | Composed effect: represents effects that can be combined
data ComposedEffect where
  SingleEffect :: (HasEffect a) => EffectActionKey -> a -> Effect -> ComposedEffect
  Sequential   :: ComposedEffect -> ComposedEffect -> ComposedEffect
  Parallel     :: ComposedEffect -> ComposedEffect -> ComposedEffect

-- | Effect composition operators  
infixr 6 `andThen`   -- Sequential composition: one effect and then another
infixr 7 `alongside` -- Parallel composition: effects happening simultaneously

-- | Sequential effect composition: apply left, then right
andThen :: ComposedEffect -> ComposedEffect -> ComposedEffect
andThen = Sequential

-- | Parallel effect composition: apply both simultaneously  
alongside :: ComposedEffect -> ComposedEffect -> ComposedEffect
alongside = Parallel

-- | Create a single effect
singleEffect :: (HasEffect a) => EffectActionKey -> a -> Effect -> ComposedEffect
singleEffect = SingleEffect

-- | Execute composed effects
executeEffect :: ComposedEffect -> SashaLambdaDSL ()
executeEffect (SingleEffect key target effect) = linkEffect key target effect
executeEffect (Sequential left right) = do
  executeEffect left
  executeEffect right
executeEffect (Parallel left right) = do
  executeEffect left
  executeEffect right