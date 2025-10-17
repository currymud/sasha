{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EDSL.Effects.HasEffect where

import           Data.Kind                     (Constraint, Type)
import           EDSL.Effects.TypeMappings     (ActionFunctionType)
import           Model.Core                    (ActionEffectKey,
                                                AgentAcquisitionActionF,
                                                AgentDirectionalStimulusActionF,
                                                AgentDirectionalStimulusContainerActionF,
                                                AgentImplicitStimulusActionF,
                                                ContainerAcquisitionActionF,
                                                ContainerDirectionalStimulusContainerActionF,
                                                Effect, Location,
                                                LocationAcquisitionActionF,
                                                LocationDirectionalStimulusActionF,
                                                LocationDirectionalStimulusContainerActionF,
                                                LocationImplicitStimulusActionF,
                                                Object,
                                                ObjectAcquisitionActionF,
                                                ObjectDirectionalStimulusActionF,
                                                PlayerKey)
import           Model.EDSL.SashaLambdaDSL     (SashaLambdaDSL,
                                                createAgentAcquisitionVerbEffect,
                                                createAgentAcquisitionVerbPhraseEffect,
                                                createAgentDirectionalContainerStimulusEffect,
                                                createAgentDirectionalStimulusEffect,
                                                createAgentImplicitStimulusEffect,
                                                createContainerAccessEffect,
                                                createContainerAccessVerbPhraseEffect,
                                                createContainerAcquisitionVerbEffect,
                                                createContainerAcquisitionVerbPhraseEffect,
                                                createContainerDirectionalContainerStimulusEffect,
                                                createLocationAcquisitionVerbEffect,
                                                createLocationAcquisitionVerbPhraseEffect,
                                                createLocationDirectionalContainerStimulusEffect,
                                                createLocationDirectionalStimulusEffect,
                                                createLocationImplicitStimulusEffect,
                                                createObjectAcquisitionVerbEffect,
                                                createObjectAcquisitionVerbPhraseEffect,
                                                createObjectDirectionalStimulusEffect,
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

instance MakeEffect SimpleAccessVerb where
  makeEffect = createContainerAccessEffect

instance MakeEffect ContainerAccessVerbPhrase where
  makeEffect = createContainerAccessVerbPhraseEffect

-- | Role-based effect creation functions
-- These allow creating Effects for role-based action types
makeAgentEffect :: AcquisitionVerb -> GID AgentAcquisitionActionF -> SashaLambdaDSL Effect
makeAgentEffect = createAgentAcquisitionVerbEffect

makeObjectEffect :: AcquisitionVerb -> GID ObjectAcquisitionActionF -> SashaLambdaDSL Effect
makeObjectEffect = createObjectAcquisitionVerbEffect

makeContainerEffect :: AcquisitionVerb -> GID ContainerAcquisitionActionF -> SashaLambdaDSL Effect
makeContainerEffect = createContainerAcquisitionVerbEffect

makeLocationEffect :: AcquisitionVerb -> GID LocationAcquisitionActionF -> SashaLambdaDSL Effect
makeLocationEffect = createLocationAcquisitionVerbEffect

-- Role-based effect creation for verb phrases
makeAgentPhraseEffect :: AcquisitionVerbPhrase -> GID AgentAcquisitionActionF -> SashaLambdaDSL Effect
makeAgentPhraseEffect = createAgentAcquisitionVerbPhraseEffect

makeObjectPhraseEffect :: AcquisitionVerbPhrase -> GID ObjectAcquisitionActionF -> SashaLambdaDSL Effect
makeObjectPhraseEffect = createObjectAcquisitionVerbPhraseEffect

makeContainerPhraseEffect :: AcquisitionVerbPhrase -> GID ContainerAcquisitionActionF -> SashaLambdaDSL Effect
makeContainerPhraseEffect = createContainerAcquisitionVerbPhraseEffect

makeLocationPhraseEffect :: AcquisitionVerbPhrase -> GID LocationAcquisitionActionF -> SashaLambdaDSL Effect
makeLocationPhraseEffect = createLocationAcquisitionVerbPhraseEffect

-- Role-based directional stimulus effect creation functions
makeAgentISEffect :: ImplicitStimulusVerb
                       -> GID AgentImplicitStimulusActionF
                       -> SashaLambdaDSL Effect
makeAgentISEffect = createAgentImplicitStimulusEffect

makeLocationISEffect :: ImplicitStimulusVerb
                          -> GID LocationImplicitStimulusActionF
                          -> SashaLambdaDSL Effect
makeLocationISEffect = createLocationImplicitStimulusEffect

makeAgentDSEffect :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusActionF -> SashaLambdaDSL Effect
makeAgentDSEffect = createAgentDirectionalStimulusEffect

makeObjectDSEffect :: DirectionalStimulusVerb -> GID ObjectDirectionalStimulusActionF -> SashaLambdaDSL Effect
makeObjectDSEffect = createObjectDirectionalStimulusEffect

makeLocationDSEffect :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusActionF -> SashaLambdaDSL Effect
makeLocationDSEffect = createLocationDirectionalStimulusEffect

makeAgentCDSEffect :: DirectionalStimulusVerb
                            -> GID AgentDirectionalStimulusContainerActionF
                            -> SashaLambdaDSL Effect
makeAgentCDSEffect = createAgentDirectionalContainerStimulusEffect

makeContainerCDSEffect :: DirectionalStimulusVerb
                                -> GID ContainerDirectionalStimulusContainerActionF
                                -> SashaLambdaDSL Effect
makeContainerCDSEffect = createContainerDirectionalContainerStimulusEffect

makeLocationCDSEffect :: DirectionalStimulusVerb
                                -> GID LocationDirectionalStimulusContainerActionF
                                -> SashaLambdaDSL Effect
makeLocationCDSEffect = createLocationDirectionalContainerStimulusEffect
