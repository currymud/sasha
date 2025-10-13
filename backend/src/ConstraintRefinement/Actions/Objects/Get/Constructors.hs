module ConstraintRefinement.Actions.Objects.Get.Constructors where
import           Control.Monad.Identity                           (Identity)
import qualified Data.Set
import           GameState                                        (addToInventoryM,
                                                                   getObjectM,
                                                                   modifySpatialRelationshipsForObjectM)
import           GameState.ActionManagement                       (processEffectsFromRegistry)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.Core                                       (AcquisitionActionF (CollectedF, LosesObjectF, ObjectNotGettableF),
                                                                   ObjectAcquisitionActionF,
                                                                   ContainerAcquisitionActionF,
                                                                   ActionEffectKey (AcquisitionalActionKey),
                                                                   ActionManagement (AVManagementKey),
                                                                   ActionManagementFunctions (ActionManagementFunctions),
                                                                   CoordinationResult (CoordinationResult, _actionEffectKeys, _computation),
                                                                   GameComputation,
                                                                   Object (_objectActionManagement),
                                                                   SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))
import           Model.GID                                        (GID)
-- Import role-based constructors and conversion functions
import           ConstraintRefinement.Actions.RoleBased.Constructors (objectCollectedF, objectNotCollectableF, containerLosesObjectF)
import           ConstraintRefinement.Actions.RoleBased.Conversions (objectToAcquisitionActionF, containerToAcquisitionActionF)

getObjectF :: GID Object -> AcquisitionActionF
getObjectF objectGID = objectToAcquisitionActionF (objectCollectedF objectGID)

getFromSupportF :: GID Object -> AcquisitionActionF
getFromSupportF supportObjGID = containerToAcquisitionActionF (containerLosesObjectF supportObjGID)

objectNotGettableF :: AcquisitionActionF
objectNotGettableF = objectToAcquisitionActionF objectNotCollectableF
