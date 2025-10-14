{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EDSL.Actions.HasAction where

import           Data.Kind                     (Constraint, Type)
import           Model.Core                    (AgentAcquisitionActionF,
                                                ObjectAcquisitionActionF, ContainerAcquisitionActionF,
                                                LocationAcquisitionActionF, ConsumptionActionF,
                                                ContainerAccessActionF, DirectionalStimulusActionF,
                                                ImplicitStimulusActionF, PosturalActionF,
                                                SomaticAccessActionF)
import           Model.EDSL.SashaLambdaDSL     (SashaLambdaDSL,
                                                declareAgentAcquisitionActionGID,
                                                declareObjectAcquisitionActionGID,
                                                declareContainerAcquisitionActionGID,
                                                declareLocationAcquisitionActionGID,
                                                declareConsumptionActionGID,
                                                declareContainerAccessActionGID,
                                                declareDirectionalStimulusActionGID,
                                                declareImplicitStimulusActionGID,
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

instance HasAction ImplicitStimulusActionF where
  declareAction = declareImplicitStimulusActionGID

instance HasAction PosturalActionF where
  declareAction = declarePosturalActionGID

instance HasAction SomaticAccessActionF where
  declareAction = declareSomaticActionGID