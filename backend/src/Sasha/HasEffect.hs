{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sasha.HasEffect where

import           Data.Kind                 (Constraint, Type)
import           Model.Core                (Effect, EffectActionKey, Location,
                                            Object, PlayerKey)
import           Model.EDSL.SashaLambdaDSL (SashaLambdaDSL, linkEffectToLocation,
                                            linkEffectToObject,
                                            linkEffectToPlayer)
import           Model.GID                 (GID)

-- | Unified interface for linking effects to any entity type
type HasEffect :: Type -> Constraint
class HasEffect a where
  linkEffect :: EffectActionKey -> a -> Effect -> SashaLambdaDSL ()

instance HasEffect (GID Location) where
  linkEffect = linkEffectToLocation

instance HasEffect (GID Object) where
  linkEffect = linkEffectToObject

instance HasEffect PlayerKey where
  linkEffect = linkEffectToPlayer