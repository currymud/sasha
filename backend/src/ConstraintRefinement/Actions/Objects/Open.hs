module ConstraintRefinement.Actions.Objects.Open where
import           Control.Monad.Identity                            (Identity)
import qualified Data.Set
import           GameState                                         (getObjectM)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs (open)
import           Model.Core                                        (ActionEffectKey (ContainerAccessActionKey),
                                                                    ActionManagement (SAConManagementKey),
                                                                    ActionManagementFunctions (ActionManagementFunctions),
                                                                    ContainerAccessActionF (ObjectContainerAccessF),
                                                                    ContainerAccessResult (ContainerAccessResult, _containerActionEffectKeys, _containerFieldEffectKeys),
                                                                    GameComputation,
                                                                    Object (_objectActionManagement))
import           Model.GID                                         (GID)


-- ToDo: get back to this next
openContainerF :: GID Object -> ContainerAccessActionF
openContainerF objectGID = ObjectContainerAccessF openit
  where
    openit :: ActionEffectKey -> GameComputation Identity ContainerAccessResult
    openit actionEffectKey = do
      actionManagement <- _objectActionManagement <$> getObjectM objectGID
      let ActionManagementFunctions actionSet = actionManagement
      -- Find the single AVManagementKey entry that matches the 'get' verb
      let getActionGIDs = [gid | SAConManagementKey verb gid <- Data.Set.toList actionSet, verb == open]
      pure $ ContainerAccessResult
        {
          _containerActionEffectKeys = map ContainerAccessActionKey getActionGIDs
        , _containerFieldEffectKeys = map ContainerAccessActionKey getActionGIDs
        }
