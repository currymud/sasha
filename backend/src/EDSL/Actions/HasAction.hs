{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EDSL.Actions.HasAction where

import           Data.Kind                     (Constraint, Type)
import           Model.Core                    (AgentAcquisitionActionF,
                                                AgentDirectionalStimulusActionF,
                                                AgentDirectionalStimulusContainerActionF,
                                                AgentImplicitStimulusActionF,
                                                ContainerAccessActionF,
                                                ContainerAcquisitionActionF,
                                                ContainerDirectionalStimulusContainerActionF,
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                DirectionalStimulusContainerActionF,
                                                ImplicitStimulusActionF,
                                                LocationAcquisitionActionF,
                                                LocationDirectionalStimulusActionF,
                                                LocationDirectionalStimulusContainerActionF,
                                                LocationImplicitStimulusActionF,
                                                ObjectAcquisitionActionF,
                                                ObjectDirectionalStimulusActionF,
                                                PosturalActionF,
                                                SomaticAccessActionF)
import           Model.EDSL.SashaLambdaDSL     (SashaLambdaDSL,
                                                declareAgentAcquisitionActionGID,
                                                declareAgentDirectionalStimulusActionGID,
                                                declareAgentDirectionalContainerStimulusActionGID,
                                                declareAgentImplicitStimulusActionGID,
                                                declareContainerAccessActionGID,
                                                declareContainerAcquisitionActionGID,
                                                declareContainerDirectionalContainerStimulusActionGID,
                                                declareConsumptionActionGID,
                                                declareDirectionalContainerActionGID,
                                                declareDirectionalStimulusActionGID,
                                                declareLocationAcquisitionActionGID,
                                                declareLocationDirectionalStimulusActionGID,
                                                declareLocationDirectionalContainerStimulusActionGID,
                                                declareLocationImplicitStimulusActionGID,
                                                declareObjectAcquisitionActionGID,
                                                declareObjectDirectionalStimulusActionGID,
                                                declarePosturalActionGID,
                                                declareSomaticActionGID)
import           Model.GID                     (GID)

-- | Unified interface for declaring actions
type HasAction :: Type -> Constraint
class HasAction actionType where
  declareAction :: actionType -> SashaLambdaDSL (GID actionType)

-- Role-based acquisition action instances
instance HasAction AgentAcquisitionActionF where
  declareAction = declareAgentAcquisitionActionGID

instance HasAction ObjectAcquisitionActionF where
  declareAction = declareObjectAcquisitionActionGID

instance HasAction ContainerAcquisitionActionF where
  declareAction = declareContainerAcquisitionActionGID

instance HasAction LocationAcquisitionActionF where
  declareAction = declareLocationAcquisitionActionGID

instance HasAction ConsumptionActionF where
  declareAction = declareConsumptionActionGID

instance HasAction ContainerAccessActionF where
  declareAction = declareContainerAccessActionGID

instance HasAction DirectionalStimulusActionF where
  declareAction = declareDirectionalStimulusActionGID

instance HasAction DirectionalStimulusContainerActionF where
  declareAction = declareDirectionalContainerActionGID

-- Role-based directional stimulus action instances
instance HasAction AgentDirectionalStimulusActionF where
  declareAction = declareAgentDirectionalStimulusActionGID

instance HasAction ObjectDirectionalStimulusActionF where
  declareAction = declareObjectDirectionalStimulusActionGID

instance HasAction LocationDirectionalStimulusActionF where
  declareAction = declareLocationDirectionalStimulusActionGID

-- Role-based directional container stimulus action instances
instance HasAction AgentDirectionalStimulusContainerActionF where
  declareAction = declareAgentDirectionalContainerStimulusActionGID

instance HasAction ContainerDirectionalStimulusContainerActionF where
  declareAction = declareContainerDirectionalContainerStimulusActionGID

instance HasAction LocationDirectionalStimulusContainerActionF where
  declareAction = declareLocationDirectionalContainerStimulusActionGID

-- Role-based implicit stimulus action instances
instance HasAction AgentImplicitStimulusActionF where
  declareAction = declareAgentImplicitStimulusActionGID

instance HasAction LocationImplicitStimulusActionF where
  declareAction = declareLocationImplicitStimulusActionGID

instance HasAction PosturalActionF where
  declareAction = declarePosturalActionGID

instance HasAction SomaticAccessActionF where
  declareAction = declareSomaticActionGID