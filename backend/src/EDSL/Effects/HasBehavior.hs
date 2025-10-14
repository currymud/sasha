{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module EDSL.Effects.HasBehavior where

import           Data.Kind                     (Constraint, Type)
import           Model.Core                    (ActionManagement (..), 
                                                AgentAcquisitionActionF,
                                                ObjectAcquisitionActionF,
                                                ContainerAcquisitionActionF,
                                                LocationAcquisitionActionF,
                                                Location, Object, Player)
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
import           EDSL.Effects.TypeMappings            (ActionFunctionType)

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

-- Old instance removed (AcquisitionVerb for AcquisitionActionF)

instance MakeBehavior SomaticAccessVerb where
  makeBehavior = SSAManagementKey

instance MakeBehavior SimpleAccessVerb where
  makeBehavior = SAConManagementKey

-- Old instance removed (AcquisitionVerbPhrase for AcquisitionActionF)

instance MakeBehavior ContainerAccessVerbPhrase where
  makeBehavior = CONManagementKey

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
