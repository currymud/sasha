{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Player.Open (openF,openDeniedF) where
import           Build.BedPuzzle.Actions.Utils                     (AcquisitionError (ContainerMissingAction, InvalidActionType, ObjectNotFound, ObjectNotGettable),
                                                                    handleAcquisitionError)
import           Control.Monad.Identity                            (Identity)
import qualified Data.Map.Strict
import           Data.Text                                         (Text, pack)
import           GameState                                         (getObjectM,
                                                                    modifyNarration,
                                                                    parseAccessPhrase)
import           GameState.ActionManagement                        (findSAForContainersKey)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs  (get)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs (open)
import           Model.GameState                                   (AccessRes (CompleteAR, SimpleAR),
                                                                    AcquisitionActionF (AcquisitionActionF, CollectedF, LosesObjectF, NotGettableF),
                                                                    AcquisitionRes (Complete, Simple),
                                                                    AcquisitionVerbActionMap,
                                                                    CompleteAccessRes (CompleteAccessRes),
                                                                    CompleteAcquisitionRes (CompleteAcquisitionRes, _caObjectKey, _caObjectPhrase, _caSupportKey, _caSupportPhrase),
                                                                    ContainerAccessActionF (CannotAccessF, InstrumentContainerAccessF, ObjectContainerAccessF, PlayerContainerAccessF),
                                                                    ContainerAccessActionMap,
                                                                    EffectActionKey,
                                                                    FinalizeAccessNotInstrumentF,
                                                                    FinalizeAcquisitionF,
                                                                    GameComputation,
                                                                    Object (_objectActionManagement),
                                                                    SearchStrategy,
                                                                    SimpleAccessRes (SimpleAccessRes, _saContainerKey, _saContainerPhrase),
                                                                    SimpleAccessSearchStrategy,
                                                                    updateActionConsequence)
import           Model.GID                                         (GID)
import           Model.Parser.Composites.Verbs                     (AcquisitionVerbPhrase,
                                                                    ContainerAccessVerbPhrase)
import           Model.Parser.GCase                                (NounKey)

openDeniedF :: ContainerAccessActionF
openDeniedF = CannotAccessF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You are in position ot be opening anything but your eyes."

openF :: ContainerAccessActionF
openF = PlayerContainerAccessF openit
  where
    openit :: EffectActionKey
               -> SimpleAccessSearchStrategy
               -> ContainerAccessActionMap
               -> ContainerAccessVerbPhrase
               ->  FinalizeAccessNotInstrumentF
               -> GameComputation Identity ()
    openit actionKey searchStrategy actionMap avp finalize = do
      case caRes of
        SimpleAR (SimpleAccessRes {..}) -> do
          osValidation <- validateObjectSearch searchStrategy _saContainerKey
          case osValidation of
            Left err' -> handleAcquisitionError err'
            Right objectGID -> do
              objectActionLookup <- lookupAccessAction objectGID actionMap
              case objectActionLookup of
                Left errM -> errM
                Right (InstrumentContainerAccessF _) -> error $ "Container " ++ show objectGID ++ " has an InstrumentContainerAccessF action, which is invalid."
                Right (PlayerContainerAccessF _) -> error $ "Container " ++ show objectGID ++ " has a PlayerContainerAccessF action, which is invalid."
                Right (CannotAccessF actionF) -> actionF
                Right (ObjectContainerAccessF actionF) -> finalize actionKey actionF
        CompleteAR (CompleteAccessRes {..}) -> error "openF: Complete Access Result not implemented."
      where
        caRes = parseAccessPhrase avp

lookupAccessAction :: GID Object
                        -> ContainerAccessActionMap
                        -> GameComputation Identity (Either (GameComputation Identity ()) ContainerAccessActionF)
lookupAccessAction objectGID actionMap = do
  actionMgmt <- _objectActionManagement <$> getObjectM objectGID
  case findSAForContainersKey open actionMgmt of
    Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence ((Data.Text.pack . show) objectGID <> " does not have a 'get' action.")
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence $ "No acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> do
         pure $ Right action


validateObjectSearch :: SimpleAccessSearchStrategy -> NounKey -> GameComputation Identity (Either AcquisitionError (GID Object))
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing        -> pure $ Left $ ObjectNotFound "You don't see that here."
    Just objectGID -> pure $ Right objectGID

