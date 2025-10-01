{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EDSL.Effects.HasEffect where

import           Data.Kind                     (Constraint, Type)
import           EDSL.Effects.TypeMappings     (ActionFunctionType)
import           Model.Core                    (ActionEffectKey, Effect,
                                                Location, Object, PlayerKey)
import           Model.EDSL.SashaLambdaDSL     (SashaLambdaDSL,
                                                createAcquisitionVerbEffect,
                                                createAcquisitionVerbPhraseEffect,
                                                createContainerAccessEffect,
                                                createContainerAccessVerbPhraseEffect,
                                                createDirectionalStimulusEffect,
                                                createImplicitStimulusEffect,
                                                linkEffectToLocation,
                                                linkEffectToObject,
                                                linkEffectToPlayer)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                SimpleAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ContainerAccessVerbPhrase)

-- | Class for creating Effects with type-safe verb-to-GID mapping
type MakeEffect :: Type -> Constraint
class MakeEffect verb where
  makeEffect :: verb -> GID (ActionFunctionType verb) -> SashaLambdaDSL Effect

-- | Unified interface for linking effects to any entity type
type HasEffect :: Type -> Constraint
class HasEffect a where
  linkEffect :: ActionEffectKey -> a -> Effect -> SashaLambdaDSL ()

instance HasEffect (GID Location) where
  linkEffect = linkEffectToLocation

instance HasEffect (GID Object) where
  linkEffect = linkEffectToObject

instance HasEffect PlayerKey where
  linkEffect = linkEffectToPlayer

instance MakeEffect ImplicitStimulusVerb where
  makeEffect = createImplicitStimulusEffect

instance MakeEffect DirectionalStimulusVerb where
  makeEffect = createDirectionalStimulusEffect

instance MakeEffect AcquisitionVerb where
  makeEffect = createAcquisitionVerbEffect

instance MakeEffect SimpleAccessVerb where
  makeEffect = createContainerAccessEffect

instance MakeEffect AcquisitionVerbPhrase where
  makeEffect = createAcquisitionVerbPhraseEffect

instance MakeEffect ContainerAccessVerbPhrase where
  makeEffect = createContainerAccessVerbPhraseEffect
