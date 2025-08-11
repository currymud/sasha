module GameState.ActionManagement where
import           Data.Maybe                    (listToMaybe)
import qualified Data.Set
import           Model.GameState               (AcquisitionActionF,
                                                ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, NPManagementKey, PPManagementKey, SSAManagementKey),
                                                ActionManagementFunctions (ActionManagementFunctions),
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                ImplicitStimulusActionF,
                                                PosturalActionF,
                                                SomaticAccessActionF)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ConsumptionVerbPhrase,
                                                PosturalVerbPhrase (NegativePosturalVerbPhrase, PositivePosturalVerbPhrase))

lookupDirectionalStimulus :: DirectionalStimulusVerb -> ActionManagementFunctions -> Maybe (GID DirectionalStimulusActionF)
lookupDirectionalStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | DSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupImplicitStimulus :: ImplicitStimulusVerb -> ActionManagementFunctions -> Maybe (GID ImplicitStimulusActionF)
lookupImplicitStimulus verb (ActionManagementFunctions actions) =
  listToMaybe [gid | ISAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupSomaticAccess :: SomaticAccessVerb -> ActionManagementFunctions -> Maybe (GID SomaticAccessActionF)
lookupSomaticAccess verb (ActionManagementFunctions actions) =
  listToMaybe [gid | SSAManagementKey v gid <- Data.Set.toList actions, v == verb]

lookupAcquisition :: AcquisitionVerbPhrase -> ActionManagementFunctions -> Maybe (GID AcquisitionActionF)
lookupAcquisition phrase (ActionManagementFunctions actions) =
  listToMaybe [gid | AAManagementKey p gid <- Data.Set.toList actions, p == phrase]

lookupConsumption :: ConsumptionVerbPhrase -> ActionManagementFunctions -> Maybe (GID ConsumptionActionF)
lookupConsumption phrase (ActionManagementFunctions actions) =
  listToMaybe [gid | CAManagementKey p gid <- Data.Set.toList actions, p == phrase]

lookupPostural :: PosturalVerbPhrase -> ActionManagementFunctions -> Maybe (GID PosturalActionF)
lookupPostural phrase (ActionManagementFunctions actions) = case phrase of
  PositivePosturalVerbPhrase verb _ ->
    listToMaybe [gid | PPManagementKey v gid <- Data.Set.toList actions, v == verb]
  NegativePosturalVerbPhrase verb _ ->
    listToMaybe [gid | NPManagementKey v gid <- Data.Set.toList actions, v == verb]
-- Convenience builders
emptyActionManagement :: ActionManagementFunctions
emptyActionManagement = ActionManagementFunctions Data.Set.empty

