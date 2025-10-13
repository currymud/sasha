module ConstraintRefinement.Actions.Objects.Get.Constructors where
-- This module now provides direct role-based constructors
-- The old AcquisitionActionF-based functions are deprecated
import           ConstraintRefinement.Actions.RoleBased.Constructors (objectCollectedF, objectNotCollectableF, containerLosesObjectF)
import           Model.Core                                          (ObjectAcquisitionActionF, ContainerAcquisitionActionF)
import           Model.GID                                           (GID)
import           Model.Core                                          (Object)

-- Role-based constructors - these are the preferred API
getObjectF :: GID Object -> ObjectAcquisitionActionF
getObjectF = objectCollectedF

getFromSupportF :: GID Object -> ContainerAcquisitionActionF
getFromSupportF = containerLosesObjectF

objectNotGettableF :: ObjectAcquisitionActionF
objectNotGettableF = objectNotCollectableF
