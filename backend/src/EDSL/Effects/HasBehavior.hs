{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module EDSL.Effects.HasBehavior where

import           Data.Kind                     (Constraint, Type)
import           EDSL.Effects.TypeMappings     (ActionFunctionType)
import           Model.Core                    (ActionManagement (..),
                                                AgentAcquisitionActionF,
                                                AgentContainerAccessActionF,
                                                AgentDirectionalStimulusActionF,
                                                AgentDirectionalStimulusContainerActionF,
                                                AgentImplicitStimulusActionF,
                                                AgentPosturalActionF,
                                                ContainerAcquisitionActionF,
                                                InstrumentContainerAccessActionF,
                                                Location,
                                                LocationAcquisitionActionF,
                                                LocationContainerAccessActionF,
                                                LocationDirectionalStimulusActionF,
                                                LocationDirectionalStimulusContainerActionF,
                                                LocationImplicitStimulusActionF,
                                                LocationPosturalActionF, Object,
                                                ObjectAcquisitionActionF,
                                                ObjectContainerAccessActionF,
                                                ObjectDirectionalStimulusActionF,
                                                ObjectDirectionalStimulusContainerActionF,
                                                Player)
import           Model.EDSL.SashaLambdaDSL     (SashaLambdaDSL,
                                                withLocationBehavior,
                                                withObjectBehavior,
                                                withPlayerBehavior)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                PositivePosturalVerb,
                                                SimpleAccessVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ContainerAccessVerbPhrase)

-- | Class for creating ActionManagement with type-safe verb-to-GID mapping
type MakeBehavior :: Type -> Constraint
class MakeBehavior verb where
  makeBehavior :: verb -> GID (ActionFunctionType verb) -> ActionManagement

-- | Unified interface for adding behaviors to any entity type
type HasBehavior :: Type -> Constraint
class HasBehavior a where
  withBehavior :: ActionManagement -> a -> SashaLambdaDSL a

instance MakeBehavior SomaticAccessVerb where
  makeBehavior = SSAManagementKey

-- SimpleAccessVerb and ContainerAccessVerbPhrase instances removed - use role-specific functions instead

instance HasBehavior Location where
  withBehavior = flip withLocationBehavior

instance HasBehavior Object where
  withBehavior = flip withObjectBehavior

instance HasBehavior Player where
  withBehavior = flip withPlayerBehavior

-- | Role-based behavior creation functions
-- These allow creating ActionManagement for role-based action types
makeAgentBehavior :: AcquisitionVerb -> GID AgentAcquisitionActionF -> ActionManagement
makeAgentBehavior = AgentAVManagementKey

makeObjectBehavior :: AcquisitionVerb -> GID ObjectAcquisitionActionF -> ActionManagement
makeObjectBehavior = ObjectAVManagementKey

makeContainerBehavior :: AcquisitionVerb -> GID ContainerAcquisitionActionF -> ActionManagement
makeContainerBehavior = ContainerAVManagementKey

makeLocationBehavior :: AcquisitionVerb -> GID LocationAcquisitionActionF -> ActionManagement
makeLocationBehavior = LocationAVManagementKey

-- Role-based behavior creation for verb phrases
makeAgentPhraseBehavior :: AcquisitionVerbPhrase -> GID AgentAcquisitionActionF -> ActionManagement
makeAgentPhraseBehavior = AgentAAManagementKey

makeObjectPhraseBehavior :: AcquisitionVerbPhrase -> GID ObjectAcquisitionActionF -> ActionManagement
makeObjectPhraseBehavior = ObjectAAManagementKey

makeContainerPhraseBehavior :: AcquisitionVerbPhrase -> GID ContainerAcquisitionActionF -> ActionManagement
makeContainerPhraseBehavior = ContainerAAManagementKey

makeLocationPhraseBehavior :: AcquisitionVerbPhrase -> GID LocationAcquisitionActionF -> ActionManagement
makeLocationPhraseBehavior = LocationAAManagementKey

-- Role-based directional stimulus behavior creation functions
makeAgentDSBehavior :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusActionF -> ActionManagement
makeAgentDSBehavior = AgentDSAManagementKey

makeObjectDSBehavior :: DirectionalStimulusVerb -> GID ObjectDirectionalStimulusActionF -> ActionManagement
makeObjectDSBehavior = ObjectDSAManagementKey

makeLocationDSBehavior :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusActionF -> ActionManagement
makeLocationDSBehavior = LocationDSAManagementKey

makeAgentCDSBehavior :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusContainerActionF -> ActionManagement
makeAgentCDSBehavior = AgentDSAContainerManagementKey

makeContainerCDSBehavior :: DirectionalStimulusVerb -> GID ObjectDirectionalStimulusContainerActionF -> ActionManagement
makeContainerCDSBehavior = ObjectDSAContainerManagementKey

makeLocationCDSBehavior :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusContainerActionF -> ActionManagement
makeLocationCDSBehavior = LocationDSAContainerManagementKey

-- Role-based implicit stimulus behavior creation functions
makeAgentISBehavior :: ImplicitStimulusVerb -> GID AgentImplicitStimulusActionF -> ActionManagement
makeAgentISBehavior = AgentISAManagementKey

makeLocationISBehavior :: ImplicitStimulusVerb -> GID LocationImplicitStimulusActionF -> ActionManagement
makeLocationISBehavior = LocationISAManagementKey

-- Role-based container access behavior creation functions
makeAgentContainerAccessBehavior :: SimpleAccessVerb -> GID AgentContainerAccessActionF -> ActionManagement
makeAgentContainerAccessBehavior = AgentSAConManagementKey

makeLocationContainerAccessBehavior :: SimpleAccessVerb -> GID LocationContainerAccessActionF -> ActionManagement
makeLocationContainerAccessBehavior = LocationSAConManagementKey

makeObjectContainerAccessBehavior :: SimpleAccessVerb -> GID ObjectContainerAccessActionF -> ActionManagement
makeObjectContainerAccessBehavior = ObjectSAConManagementKey

makeInstrumentContainerAccessBehavior :: SimpleAccessVerb -> GID InstrumentContainerAccessActionF -> ActionManagement
makeInstrumentContainerAccessBehavior = InstrumentSAConManagementKey

-- Role-based container access verb phrase behavior creation functions
makeAgentContainerAccessPhraseBehavior :: ContainerAccessVerbPhrase -> GID AgentContainerAccessActionF -> ActionManagement
makeAgentContainerAccessPhraseBehavior = AgentConManagementKey

makeLocationContainerAccessPhraseBehavior :: ContainerAccessVerbPhrase -> GID LocationContainerAccessActionF -> ActionManagement
makeLocationContainerAccessPhraseBehavior = LocationConManagementKey

makeObjectContainerAccessPhraseBehavior :: ContainerAccessVerbPhrase -> GID ObjectContainerAccessActionF -> ActionManagement
makeObjectContainerAccessPhraseBehavior = ObjectConManagementKey

makeInstrumentContainerAccessPhraseBehavior :: ContainerAccessVerbPhrase -> GID InstrumentContainerAccessActionF -> ActionManagement
makeInstrumentContainerAccessPhraseBehavior = InstrumentConManagementKey

-- Role-based postural behavior creation functions
makeAgentPosturalBehavior :: PositivePosturalVerb -> GID AgentPosturalActionF -> ActionManagement
makeAgentPosturalBehavior = AgentPPManagementKey

makeLocationPosturalBehavior :: PositivePosturalVerb -> GID LocationPosturalActionF -> ActionManagement
makeLocationPosturalBehavior = LocationPPManagementKey
