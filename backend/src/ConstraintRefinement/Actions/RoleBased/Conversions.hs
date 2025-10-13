module ConstraintRefinement.Actions.RoleBased.Conversions where

import Model.Core (AcquisitionActionF(..), AgentAcquisitionActionF(..), 
                   ObjectAcquisitionActionF(..), ContainerAcquisitionActionF(..), 
                   LocationAcquisitionActionF(..))

-- Conversion functions from role-based types to original AcquisitionActionF
-- These allow gradual migration while maintaining backwards compatibility

toAcquisitionActionF :: AgentAcquisitionActionF -> AcquisitionActionF
toAcquisitionActionF (AgentAcquiresF acquisitionF) = AcquisitionActionF acquisitionF
toAcquisitionActionF (AgentCannotAcquireF effectF) = CannotAcquireF effectF

objectToAcquisitionActionF :: ObjectAcquisitionActionF -> AcquisitionActionF  
objectToAcquisitionActionF (ObjectCollectedF coordinationF) = CollectedF coordinationF
objectToAcquisitionActionF (ObjectNotCollectableF effectF) = ObjectNotGettableF effectF

containerToAcquisitionActionF :: ContainerAcquisitionActionF -> AcquisitionActionF
containerToAcquisitionActionF (ContainerLosesObjectF objToCoordF) = LosesObjectF objToCoordF
containerToAcquisitionActionF (ContainerCannotReleaseF effectF) = ObjectNotGettableF effectF

locationToAcquisitionActionF :: LocationAcquisitionActionF -> AcquisitionActionF
locationToAcquisitionActionF (LocationAcquisitionActionF gameComp) = AcquisitionActionF (\_ _ _ _ -> gameComp)
locationToAcquisitionActionF (LocationCannotAcquireF effectF) = CannotAcquireF effectF