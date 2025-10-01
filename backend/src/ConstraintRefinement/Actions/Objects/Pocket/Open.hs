module ConstraintRefinement.Actions.Objects.Pocket.Open (notEvenOpenF, pocketOutOfReachF) where
import           Control.Monad.Identity     (Identity)
import           GameState                  (getObjectM)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (ActionEffectKey (ContainerAccessActionKey),
                                             ActionManagementFunctions,
                                             ContainerAccessActionF (CannotAccessF, ObjectContainerAccessF),
                                             GameComputation,
                                             Object (_objectActionManagement))
import           Model.GID                  (GID)

notEvenOpenF :: GID Object -> ContainerAccessActionF
notEvenOpenF oid = ObjectContainerAccessF notEvenOpen
  where
    notEvenOpen :: (ActionManagementFunctions -> Maybe (GID ContainerAccessActionF))
                     -> GameComputation Identity ActionEffectKey
    notEvenOpen actionGIDF = do
      availableActions <- _objectActionManagement <$> getObjectM oid
      case actionGIDF availableActions of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show oid)
        Just actionGID -> do
          pure $ ContainerAccessActionKey actionGID

pocketOutOfReachF :: GID Object -> ContainerAccessActionF
pocketOutOfReachF oid = CannotAccessF outOfReach
  where
    outOfReach :: (ActionManagementFunctions -> Maybe (GID ContainerAccessActionF))
                    -> GameComputation Identity ()
    outOfReach actionGIDF = do
      availableActions <- _objectActionManagement <$> getObjectM oid
      case actionGIDF availableActions of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show oid)
        Just actionGID ->
          let actionKey = ContainerAccessActionKey actionGID
          in processEffectsFromRegistry actionKey
