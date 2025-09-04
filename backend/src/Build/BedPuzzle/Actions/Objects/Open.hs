module Build.BedPuzzle.Actions.Open where
import           Control.Monad.Identity                            (Identity)
import qualified Data.Set
import           GameState                                         (getObjectM)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs (open)
import           Model.GameState                                   (ActionManagement (SAConManagementKey),
                                                                    ActionManagementFunctions (ActionManagementFunctions),
                                                                    ContainerAccessActionF (ObjectContainerAccessF),
                                                                    ContainerAccessResult (ContainerAccessResult, _containerActionEffectKeys, _containerFieldEffectKeys),
                                                                    EffectActionKey (ContainerAccessActionKey),
                                                                    GameComputation,
                                                                    Object (_objectActionManagement))
import           Model.GID                                         (GID)

openContainerF :: GID Object -> ContainerAccessActionF
openContainerF objectGID = ObjectContainerAccessF getit
  where
    getit :: GameComputation Identity ContainerAccessResult
    getit = do
      actionManagement <- _objectActionManagement <$> getObjectM objectGID
      let ActionManagementFunctions actionSet = actionManagement
      -- Find the single AVManagementKey entry that matches the 'get' verb
      let getActionGIDs = [gid | SAConManagementKey verb gid <- Data.Set.toList actionSet, verb == open]
      pure $ ContainerAccessResult
        {
          _containerActionEffectKeys = map ContainerAccessActionKey getActionGIDs
        , _containerFieldEffectKeys = map ContainerAccessActionKey getActionGIDs
        }
