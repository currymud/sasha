module ConstraintRefinement.Actions.Objects.Open where
import           Control.Monad.Identity                            (Identity)
import qualified Data.Set
import           GameState                                         (getObjectM)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs (open)
import           Model.Core                                        (ActionManagement (SAConManagementKey),
                                                                    ActionManagementFunctions (ActionManagementFunctions),
                                                                    ContainerAccessActionF (ObjectContainerAccessF),
                                                                    ContainerAccessResult (ContainerAccessResult, _containerActionEffectKeys, _containerFieldEffectKeys),
                                                                    ActionEffectKey (ContainerAccessActionKey),
                                                                    GameComputation,
                                                                    Object (_objectActionManagement))
import           Model.GID                                         (GID)

openContainerF :: GID Object -> ContainerAccessActionF
openContainerF objectGID = ObjectContainerAccessF openit
  where
    openit :: GameComputation Identity ContainerAccessResult
    openit = do
      actionManagement <- _objectActionManagement <$> getObjectM objectGID
      let ActionManagementFunctions actionSet = actionManagement
      -- Find the single AVManagementKey entry that matches the 'get' verb
      let getActionGIDs = [gid | SAConManagementKey verb gid <- Data.Set.toList actionSet, verb == open]
      pure $ ContainerAccessResult
        {
          _containerActionEffectKeys = map ContainerAccessActionKey getActionGIDs
        , _containerFieldEffectKeys = map ContainerAccessActionKey getActionGIDs
        }
