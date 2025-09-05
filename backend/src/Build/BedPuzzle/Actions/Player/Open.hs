{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Player.Open () where
import           Build.BedPuzzle.Actions.Utils                    (AcquisitionError (ContainerMissingAction, InvalidActionType, ObjectNotFound, ObjectNotGettable),
                                                                   handleAcquisitionError)
import           Control.Monad.Identity                           (Identity)
import qualified Data.Map.Strict
import           Data.Text                                        (Text, pack)
import           Debug.Trace                                      (trace)
import           GameState                                        (getObjectM,
                                                                   modifyNarration,
                                                                   parseAccessPhrase)
import           GameState.ActionManagement                       (findAVKey)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.GameState                                  (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                                   AcquisitionRes (Complete, Simple),
                                                                   AcquisitionVerbActionMap,
                                                                   CompleteAcquisitionRes (CompleteAcquisitionRes, _caObjectKey, _caObjectPhrase, _caSupportKey, _caSupportPhrase),
                                                                   ContainerAccessActionF (CannotAccessF, PlayerContainerAccessF),
                                                                   ContainerAccessActionMap,
                                                                   EffectActionKey,
                                                                   FinalizeAcquisitionF,
                                                                   GameComputation,
                                                                   Object (_objectActionManagement),
                                                                   SearchStrategy,
                                                                   SimpleAcquisitionRes (SimpleAcquisitionRes, _saObjectKey, _saObjectPhrase),
                                                                   updateActionConsequence)
import           Model.GID                                        (GID)
import           Model.Parser.Composites.Verbs                    (AcquisitionVerbPhrase,
                                                                   ContainerAccessVerbPhrase)
import           Model.Parser.GCase                               (NounKey)

getDeniedF :: ContainerAccessActionF
getDeniedF = CannotAccessF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You are in position ot be opening anything but your eyes."


-- ToDo: refactor to remove dead code.
openF :: ContainerAccessActionF
openF = PlayerContainerAccessF openit
  where
    openit :: EffectActionKey
               -> ContainerAccessActionMap
               -> SearchStrategy
               -> ContainerAccessVerbPhrase
               -> FinalizeAcquisitionF
               -> GameComputation Identity ()
    openit actionKey actionMap searchStrategy avp finalize = do
      case caRes of
        Simple (SimpleAcquisitionRes {..}) -> do
          osValidation <- validateObjectSearch searchStrategy _saObjectKey
          case osValidation of
            Left err' -> handleAcquisitionError err'
            Right (objectGID, containerGID) -> do
              objectActionLookup <- lookupAcquisitionAction objectGID actionMap ("Object " <> (Data.Text.pack . show) objectGID <> ":")
              trace ("DEBUG: getF - objectGID=" ++ show objectGID ++ " lookup result: ") $ pure ()
              case objectActionLookup of
                Left err' -> handleAcquisitionError err'
                Right (NotGettableF objectNotGettableF) -> objectNotGettableF
                Right (CollectedF objectActionF) -> do
                  containerActionLookup <- lookupAcquisitionAction containerGID actionMap ("Container " <> (Data.Text.pack . show) containerGID <> ":")
                  case containerActionLookup of
                    Left err' -> handleAcquisitionError err'
                    Right (NotGettableF cannotGetFromF) -> cannotGetFromF
                    Right (LosesObjectF containerActionF) -> finalize actionKey containerGID objectGID objectActionF containerActionF
                    Right _ -> handleAcquisitionError $ InvalidActionType $ "Container " <> (Data.Text.pack . show) containerGID <> " does not have a LosesObjectF action."
                Right _ -> handleAcquisitionError $ ObjectNotGettable $ "Object " <> (Data.Text.pack . show) objectGID <> " is not gettable."
        Complete (CompleteAcquisitionRes {..}) -> do
          osValidation <- validateObjectSearch searchStrategy _caObjectKey
          case osValidation of
            Left err' -> handleAcquisitionError err'
            Right (objectGID, containerGID) -> do
              objectActionLookup <- lookupAcquisitionAction objectGID actionMap ("Object " <> (Data.Text.pack . show) objectGID <> ":")
              case objectActionLookup of
                Left err' -> handleAcquisitionError err'
                Right (NotGettableF objectNotGettableF) -> objectNotGettableF
                Right (CollectedF objectActionF)-> do
                  containerActionLookup <- lookupAcquisitionAction containerGID actionMap ("Container : " <> (Data.Text.pack . show) containerGID <> ":")
                  case containerActionLookup of
                    Left err' -> handleAcquisitionError err'
                    Right (NotGettableF cannotGetFromF) -> cannotGetFromF
                    Right (LosesObjectF containerActionF) -> finalize actionKey containerGID objectGID objectActionF containerActionF
                    Right _ -> handleAcquisitionError $ InvalidActionType $ "Container " <> (Data.Text.pack . show) containerGID <> " does not have a LosesObjectF action."
                Right _ -> handleAcquisitionError $ ObjectNotGettable $ "Object " <> (Data.Text.pack . show) objectGID <> " is not gettable."
      where
        caRes = parseAccessPhrase avp

validateObjectSearch :: SearchStrategy -> NounKey -> GameComputation Identity (Either AcquisitionError (GID Object, GID Object))
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing -> pure $ Left $ ObjectNotFound "You don't see that here."
    Just (objectGID, containerGID) -> pure $ Right (objectGID, containerGID)
