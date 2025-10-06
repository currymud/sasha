module ConstraintRefinement.Actions.Objects.Robe.Get (getRobeDeniedF, alreadyHaveRobeF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (getObjectM, modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey (AcquisitionalActionKey),
                                             ActionManagementFunctions,
                                             GameComputation,
                                             Object (_objectActionManagement))
import           Model.GID                  (GID)

alreadyHaveRobeF :: GID Object -> AcquisitionActionF
alreadyHaveRobeF oid = NotGettableF haveRobe
  where
    haveRobe :: (ActionManagementFunctions -> Maybe (GID AcquisitionActionF))
                  -> GameComputation Identity ActionEffectKey
    haveRobe lookupActionF = do
      actionManagement <- _objectActionManagement <$> getObjectM oid
      case lookupActionF actionManagement of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show oid)
        Just actionGID ->
          let actionKey = AcquisitionalActionKey actionGID
          in pure actionKey

getRobeDeniedF :: GID Object ->  AcquisitionActionF
getRobeDeniedF oid = NotGettableF denied
  where
    denied :: (ActionManagementFunctions -> Maybe (GID AcquisitionActionF))
                  -> GameComputation Identity ActionEffectKey
    denied lookupActionF = do
      actionManagement <- _objectActionManagement <$> getObjectM oid
      case lookupActionF actionManagement of
        Nothing -> error ("Programmer Error: No container access action found for object " ++ show oid)
        Just actionGID ->
          let actionKey = AcquisitionalActionKey actionGID
          in pure actionKey

