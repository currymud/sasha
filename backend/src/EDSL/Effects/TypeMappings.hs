{-# LANGUAGE TypeFamilies #-}

module EDSL.Effects.TypeMappings where

import           Data.Kind                  (Type)
import           Model.Core                 (AgentAcquisitionActionF,
                                             AgentContainerAccessActionF,
                                             AgentDirectionalStimulusContainerActionF,
                                             AgentSomaticAccessActionF,
                                             ContainerAcquisitionActionF,
                                             InstrumentContainerAccessActionF,
                                             LocationAcquisitionActionF,
                                             LocationContainerAccessActionF,
                                             LocationDirectionalStimulusContainerActionF,
                                             ObjectAcquisitionActionF,
                                             ObjectContainerAccessActionF,
                                             ObjectDirectionalStimulusContainerActionF)
import           Model.Parser.Atomics.Verbs (SomaticAccessVerb)

-- | Type family mapping verb types to their corresponding action function types
type ActionFunctionType :: Type -> Type
type family ActionFunctionType (verb :: Type) :: Type where
  ActionFunctionType SomaticAccessVerb = AgentSomaticAccessActionF
  -- Role-based acquisition action type mappings
  -- These allow the type system to understand role-based actions
  ActionFunctionType AgentAcquisitionActionF = AgentAcquisitionActionF
  ActionFunctionType ObjectAcquisitionActionF = ObjectAcquisitionActionF
  ActionFunctionType ContainerAcquisitionActionF = ContainerAcquisitionActionF
  ActionFunctionType LocationAcquisitionActionF = LocationAcquisitionActionF
  -- Role-based container access action type mappings
  ActionFunctionType AgentContainerAccessActionF = AgentContainerAccessActionF
  ActionFunctionType LocationContainerAccessActionF = LocationContainerAccessActionF
  ActionFunctionType ObjectContainerAccessActionF = ObjectContainerAccessActionF
  ActionFunctionType InstrumentContainerAccessActionF = InstrumentContainerAccessActionF
  -- Role-based directional container stimulus action type mappings
  ActionFunctionType AgentDirectionalStimulusContainerActionF = AgentDirectionalStimulusContainerActionF
  ActionFunctionType ObjectDirectionalStimulusContainerActionF = ObjectDirectionalStimulusContainerActionF
  ActionFunctionType LocationDirectionalStimulusContainerActionF = LocationDirectionalStimulusContainerActionF
