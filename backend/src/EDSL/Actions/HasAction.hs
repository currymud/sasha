{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EDSL.Actions.HasAction where

import           Data.Kind                 (Constraint, Type)
import           Model.Core                (AgentAcquisitionActionF,
                                            AgentContainerAccessActionF,
                                            AgentDirectionalStimulusActionF,
                                            AgentDirectionalStimulusContainerActionF,
                                            AgentImplicitStimulusActionF,
                                            AgentPosturalActionF,
                                            ConsumptionActionF,
                                            ContainerAcquisitionActionF,
                                            ContainerDirectionalStimulusContainerActionF,
                                            InstrumentContainerAccessActionF,
                                            LocationAcquisitionActionF,
                                            LocationContainerAccessActionF,
                                            LocationDirectionalStimulusActionF,
                                            LocationDirectionalStimulusContainerActionF,
                                            LocationImplicitStimulusActionF,
                                            LocationPosturalActionF,
                                            ObjectAcquisitionActionF,
                                            ObjectContainerAccessActionF,
                                            ObjectDirectionalStimulusActionF,
                                            PosturalActionF,
                                            SomaticAccessActionF)
import           Model.EDSL.SashaLambdaDSL (SashaLambdaDSL,
                                            declareAgentAcquisitionActionGID,
                                            declareAgentContainerAccessActionGID,
                                            declareAgentDirectionalContainerStimulusActionGID,
                                            declareAgentDirectionalStimulusActionGID,
                                            declareAgentImplicitStimulusActionGID,
                                            declareAgentPosturalActionGID,
                                            declareConsumptionActionGID,
                                            declareContainerAcquisitionActionGID,
                                            declareContainerDirectionalContainerStimulusActionGID,
                                            declareInstrumentContainerAccessActionGID,
                                            declareLocationAcquisitionActionGID,
                                            declareLocationContainerAccessActionGID,
                                            declareLocationDirectionalContainerStimulusActionGID,
                                            declareLocationDirectionalStimulusActionGID,
                                            declareLocationImplicitStimulusActionGID,
                                            declareLocationPosturalActionGID,
                                            declareObjectAcquisitionActionGID,
                                            declareObjectContainerAccessActionGID,
                                            declareObjectDirectionalStimulusActionGID,
                                            declarePosturalActionGID,
                                            declareSomaticActionGID)
import           Model.GID                 (GID)

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

-- Role-based container access action instances
instance HasAction AgentContainerAccessActionF where
  declareAction = declareAgentContainerAccessActionGID

instance HasAction LocationContainerAccessActionF where
  declareAction = declareLocationContainerAccessActionGID

instance HasAction ObjectContainerAccessActionF where
  declareAction = declareObjectContainerAccessActionGID

instance HasAction InstrumentContainerAccessActionF where
  declareAction = declareInstrumentContainerAccessActionGID

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

-- Role-based postural action instances
instance HasAction AgentPosturalActionF where
  declareAction = declareAgentPosturalActionGID

instance HasAction LocationPosturalActionF where
  declareAction = declareLocationPosturalActionGID

instance HasAction SomaticAccessActionF where
  declareAction = declareSomaticActionGID
