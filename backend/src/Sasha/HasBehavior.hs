{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Sasha.HasBehavior where

import           Data.Kind                     (Constraint, Type)
import           Model.Core                    (ActionManagement (..), Location,
                                                Object, Player)
import           Model.EDSL.SashaLambdaDSL     (SashaLambdaDSL,
                                                withLocationBehavior,
                                                withObjectBehavior,
                                                withPlayerBehavior)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                SimpleAccessVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ContainerAccessVerbPhrase)
import           Sasha.TypeMappings            (ActionFunctionType)

-- | Class for creating ActionManagement with type-safe verb-to-GID mapping
type MakeBehavior :: Type -> Constraint
class MakeBehavior verb where
  makeBehavior :: verb -> GID (ActionFunctionType verb) -> ActionManagement

-- | Unified interface for adding behaviors to any entity type
type HasBehavior :: Type -> Constraint
class HasBehavior a where
  withBehavior :: ActionManagement -> a -> SashaLambdaDSL a

instance MakeBehavior ImplicitStimulusVerb where
  makeBehavior = ISAManagementKey

instance MakeBehavior DirectionalStimulusVerb where
  makeBehavior = DSAManagementKey

instance MakeBehavior AcquisitionVerb where
  makeBehavior = AVManagementKey

instance MakeBehavior SomaticAccessVerb where
  makeBehavior = SSAManagementKey

instance MakeBehavior SimpleAccessVerb where
  makeBehavior = SAConManagementKey

instance MakeBehavior AcquisitionVerbPhrase where
  makeBehavior = AAManagementKey

instance MakeBehavior ContainerAccessVerbPhrase where
  makeBehavior = CONManagementKey

instance HasBehavior Location where
  withBehavior = flip withLocationBehavior

instance HasBehavior Object where
  withBehavior = flip withObjectBehavior

instance HasBehavior Player where
  withBehavior = flip withPlayerBehavior
