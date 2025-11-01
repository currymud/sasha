{-# LANGUAGE TypeFamilies #-}

module EDSL.Effects.TypeMappings where

import           Data.Kind                     (Type)
import           Model.Core                    (AgentAcquisitionActionF,
                                                AgentContainerAccessActionF,
                                                AgentDirectionalStimulusContainerActionF,
                                                ContainerAcquisitionActionF,
                                                InstrumentContainerAccessActionF,
                                                LocationAcquisitionActionF,
                                                LocationContainerAccessActionF,
                                                LocationDirectionalStimulusContainerActionF,
                                                ObjectAcquisitionActionF,
                                                ObjectContainerAccessActionF,
                                                ObjectDirectionalStimulusContainerActionF,
                                                SomaticAccessActionF)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                DirectionalStimulusVerb,
                                                SimpleAccessVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ContainerAccessVerbPhrase)

-- | Type family mapping verb types to their corresponding action function types
type ActionFunctionType :: Type -> Type
type family ActionFunctionType (verb :: Type) :: Type where
  ActionFunctionType SomaticAccessVerb = SomaticAccessActionF
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
