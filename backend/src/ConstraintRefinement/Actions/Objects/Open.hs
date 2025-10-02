module ConstraintRefinement.Actions.Objects.Open where
import           Control.Monad.Identity (Identity)
import           GameState              (getObjectM)
import           Model.Core             (ActionEffectKey (ContainerAccessActionKey),
                                         ActionManagementFunctions,
                                         ContainerAccessActionF (ObjectContainerAccessF),
                                         GameComputation,
                                         Object (_objectActionManagement))
import           Model.GID              (GID)


openContainerF :: GID Object -> ContainerAccessActionF
openContainerF objectGID = ObjectContainerAccessF openit
  where
    openit :: (ActionManagementFunctions -> Maybe (GID ContainerAccessActionF))
              -> GameComputation Identity ActionEffectKey
    openit lookupActionF = do
      actionManagement <- _objectActionManagement <$> getObjectM objectGID
      case lookupActionF actionManagement of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show objectGID)
        Just actionGID ->
          let actionKey = ContainerAccessActionKey actionGID
          in pure actionKey
