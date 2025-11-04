module ConstraintRefinement.Actions.Get where

import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AgentAcquisitionActionF (AgentAcquisitionActionF),
                                             ContainerAcquisitionActionF (ContainerCollectedFromF),
                                             ObjectAcquisitionActionF (ObjectCollectedF))


getF :: AgentAcquisitionActionF
getF = AgentAcquisitionActionF processEffectsFromRegistry

gettableF :: ObjectAcquisitionActionF
gettableF = ObjectCollectedF processEffectsFromRegistry

getFromF :: ContainerAcquisitionActionF
getFromF = ContainerCollectedFromF processEffectsFromRegistry
