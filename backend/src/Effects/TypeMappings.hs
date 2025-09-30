{-# LANGUAGE TypeFamilies #-}

module Effects.TypeMappings where

import           Data.Kind                     (Type)
import           Model.Core                    (AcquisitionActionF,
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
