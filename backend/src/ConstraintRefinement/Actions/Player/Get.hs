module ConstraintRefinement.Actions.Player.Get where

import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AgentAcquisitionActionF (AgentAcquisitionActionF))


getF :: AgentAcquisitionActionF
getF = AgentAcquisitionActionF processEffectsFromRegistry

