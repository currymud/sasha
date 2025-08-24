{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Get (getF,getDeniedF) where
import           Build.BedPuzzle.Actions.Utils                    (AcquisitionError (ContainerMissingAction, InvalidActionType, ObjectNotFound, ObjectNotGettable),
                                                                   handleAcquisitionError)
import           Control.Monad.Identity                           (Identity)
import qualified Data.Map.Strict
import           Data.Text                                        (Text, pack)
import           GameState                                        (getObjectM,
                                                                   modifyNarration,
                                                                   parseAcquisitionPhrase)
import           GameState.ActionManagement                       (findAVKey)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs (get)
import           Model.GameState                                  (AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                                   AcquisitionRes (Complete, Simple),
                                                                   AcquisitionVerbActionMap,
                                                                   ActionKey ,
                                                                   CompleteAcquisitionRes (CompleteAcquisitionRes, _caObjectKey, _caObjectPhrase, _caSupportKey, _caSupportPhrase),
                                                                   FinalizeAcquisitionF,
                                                                   GameComputation,
                                                                   Object (_objectActionManagement),
                                                                   SearchStrategy,
                                                                   SimpleAcquisitionRes (SimpleAcquisitionRes, _saObjectKey, _saObjectPhrase),
                                                                   updateActionConsequence)
import           Model.GID                                        (GID)
import           Model.Parser.Composites.Verbs                    (ConsumptionVerbPhrase (AcquisitionVerbPhrase)
import           Model.Parser.GCase                               (NounKey)
getDeniedF :: AcquisitionActionF
getDeniedF = NotGettableF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You try but feel dizzy and have to lay back down"

getF :: AcquisitionActionF
getF = AcquisitionActionF getit
  where
    getit :: ActionKey
               -> AcquisitionVerbActionMap
               -> SearchStrategy
               -> AcquisitionVerbPhrase
               -> FinalizeAcquisitionF
               -> GameComputation Identity ()
    getit actionKey actionMap searchStrategy avp finalize = do
      case ares of
        Simple (SimpleAcquisitionRes {..}) -> do
          osValidation <- validateObjectSearch searchStrategy _saObjectKey
          case osValidation of
            Left err' -> handleAcquisitionError err'
            Right (objectGID, containerGID) -> do
              objectActionLookup <- lookupAcquisitionAction objectGID actionMap ("Object " <> (Data.Text.pack . show) objectGID <> ":")
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
        ares = parseAcquisitionPhrase avp

validateObjectSearch :: SearchStrategy -> NounKey -> GameComputation Identity (Either AcquisitionError (GID Object, GID Object))
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing -> pure $ Left $ ObjectNotFound "You don't see that here."
    Just (objectGID, containerGID) -> pure $ Right (objectGID, containerGID)

lookupAcquisitionAction :: GID Object
                             -> AcquisitionVerbActionMap
                             -> Text
                             -> GameComputation Identity (Either AcquisitionError AcquisitionActionF)
lookupAcquisitionAction objectGID actionMap contextDescription = do
  actionMgmt <- _objectActionManagement <$> getObjectM objectGID
  case findAVKey get actionMgmt of
    Nothing -> pure $ Left $ ContainerMissingAction $ contextDescription <> " " <> (Data.Text.pack . show) objectGID <> " does not have a 'get' action."
    Just actionGID ->
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> pure $ Left $ InvalidActionType $ "No acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> pure $ Right action
