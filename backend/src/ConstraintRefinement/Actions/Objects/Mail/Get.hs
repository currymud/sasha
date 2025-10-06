{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Objects.Mail.Get (getMailDeniedF,
                                                      alreadyHaveMailF,
                                                      getMailDizzyF) where
import           Control.Monad.Identity (Identity)
import           GameState              (getObjectM)
import           Model.Core             (AcquisitionActionF (NotGettableF),
                                         ActionEffectKey (AcquisitionalActionKey),
                                         ActionManagementFunctions,
                                         GameComputation,
                                         Object (_objectActionManagement))
import           Model.GID              (GID)

alreadyHaveMailF :: GID Object -> AcquisitionActionF
alreadyHaveMailF oid = NotGettableF denied
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
--    msg :: Text
--    msg = "You are already have your mail. it'll probably be important later."

getMailDeniedF :: GID Object -> AcquisitionActionF
getMailDeniedF oid = NotGettableF denied
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

--    msg :: Text
--    msg = "You can't reach it from your bed. You need to get up first."

getMailDizzyF :: GID Object -> AcquisitionActionF
getMailDizzyF oid = NotGettableF denied
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

--    msg :: Text
--    msg = "You stand up to go to the table, but you are still a bit dizzy and lay back down"
