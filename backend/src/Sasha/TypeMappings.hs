{-# LANGUAGE TypeFamilies #-}

module Sasha.TypeMappings where

import Model.Core (ImplicitStimulusActionF, DirectionalStimulusActionF, 
                   AcquisitionActionF, ContainerAccessActionF, SomaticAccessActionF)
import Model.Parser.Atomics.Verbs (ImplicitStimulusVerb, DirectionalStimulusVerb, AcquisitionVerb, 
                                   SomaticAccessVerb, SimpleAccessVerb)
import Model.Parser.Composites.Verbs (AcquisitionVerbPhrase, ContainerAccessVerbPhrase)

-- | Type family mapping verb types to their corresponding action function types
type family ActionFunctionType (verb :: *) :: * where
  ActionFunctionType ImplicitStimulusVerb = ImplicitStimulusActionF
  ActionFunctionType DirectionalStimulusVerb = DirectionalStimulusActionF  
  ActionFunctionType AcquisitionVerb = AcquisitionActionF
  ActionFunctionType SomaticAccessVerb = SomaticAccessActionF
  ActionFunctionType SimpleAccessVerb = ContainerAccessActionF
  ActionFunctionType AcquisitionVerbPhrase = AcquisitionActionF
  ActionFunctionType ContainerAccessVerbPhrase = ContainerAccessActionF