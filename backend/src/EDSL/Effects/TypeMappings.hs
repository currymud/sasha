{-# LANGUAGE TypeFamilies #-}

module EDSL.Effects.TypeMappings where

import           Data.Kind                     (Type)
import           Model.Core                    (AcquisitionActionF,
                                                AgentAcquisitionActionF,
                                                ObjectAcquisitionActionF,
                                                ContainerAcquisitionActionF,
                                                LocationAcquisitionActionF,
                                                ContainerAccessActionF,
                                                DirectionalStimulusActionF,
                                                ImplicitStimulusActionF,
                                                SomaticAccessActionF)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                SimpleAccessVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ContainerAccessVerbPhrase)

-- | Type family mapping verb types to their corresponding action function types
type ActionFunctionType :: Type -> Type
type family ActionFunctionType (verb :: Type) :: Type where
  ActionFunctionType ImplicitStimulusVerb = ImplicitStimulusActionF
  ActionFunctionType DirectionalStimulusVerb = DirectionalStimulusActionF
  ActionFunctionType AcquisitionVerb = AcquisitionActionF
  ActionFunctionType SomaticAccessVerb = SomaticAccessActionF
  ActionFunctionType SimpleAccessVerb = ContainerAccessActionF
  ActionFunctionType AcquisitionVerbPhrase = AcquisitionActionF
  ActionFunctionType ContainerAccessVerbPhrase = ContainerAccessActionF
  -- Role-based acquisition action type mappings
  -- These allow the type system to understand role-based actions
  ActionFunctionType AgentAcquisitionActionF = AgentAcquisitionActionF
  ActionFunctionType ObjectAcquisitionActionF = ObjectAcquisitionActionF
  ActionFunctionType ContainerAcquisitionActionF = ContainerAcquisitionActionF
  ActionFunctionType LocationAcquisitionActionF = LocationAcquisitionActionF
