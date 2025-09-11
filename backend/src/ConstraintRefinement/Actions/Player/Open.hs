module ConstraintRefinement.Actions.Player.Open where
import           ConstraintRefinement.Actions.Utils                (AcquisitionError (ObjectNotFound),
                                                                    handleAcquisitionError)
import           Control.Monad.Identity                            (Identity)
import qualified Data.Map.Strict
import           Data.Set                                          (Set)
import           Data.Text                                         (Text, pack)
import           GameState                                         (getObjectM,
                                                                    modifyNarration,
                                                                    parseAccessPhrase,
                                                                    updateActionConsequence)
import           GameState.ActionManagement                        (findSAForContainersKey)
import           GameState.Perception                              (youSeeM)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs (openSA)
import           Model.Actions.Results                             (AccessRes (CompleteAR, SimpleAR),
                                                                    CompleteAccessRes (CompleteAccessRes),
                                                                    SimpleAccessRes (SimpleAccessRes, _saContainerKey))
import           Model.Core                                        (ActionEffectMap (ActionEffectMap),
                                                                    ContainerAccessActionF (CannotAccessF, InstrumentContainerAccessF, ObjectContainerAccessF, PlayerContainerAccessF),
                                                                    ContainerAccessActionMap,
                                                                    EffectKey,
                                                                    EffectTargetKey,
                                                                    FinalizeAccessNotInstrumentF,
                                                                    GameComputation,
                                                                    Object (_objectActionManagement),
                                                                    SimpleAccessSearchStrategy,
                                                                    SomaticAccessActionF (SomaticAccessActionF),
                                                                    SystemEffectKey,
                                                                    SystemEffectRegistry)
import           Model.GID                                         (GID)
import           Model.Parser.Composites.Verbs                     (ContainerAccessVerbPhrase)
import           Model.Parser.GCase                                (NounKey)


openDeniedF :: ContainerAccessActionF
openDeniedF = CannotAccessF denied
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg'
    msg' :: Text
    msg' = "You are in position to not be opening anything but your eyes."

openEyesDenied :: SomaticAccessActionF
openEyesDenied = SomaticAccessActionF (const (const (const (const denied))))
 where
   denied :: GameComputation Identity ()
   denied = modifyNarration $ updateActionConsequence msg
   msg :: Text
   msg = "They're already open, relax."

openEyes :: SomaticAccessActionF
openEyes = SomaticAccessActionF opened
 where
   opened :: Set EffectTargetKey
             -> [SystemEffectKey]
             ->  ActionEffectMap
             ->  SystemEffectRegistry
             -> GameComputation Identity ()
   opened effectKeys _ (ActionEffectMap effectMap) _ = do
     lookEffectKeys <- youSeeM

     pure ()

msg :: Text
msg = "You open your eyes, and the world comes into focus."

openF :: ContainerAccessActionF
openF = PlayerContainerAccessF openit
  where
    openit :: [EffectKey]
               -> SimpleAccessSearchStrategy
               -> ContainerAccessActionMap
               -> ContainerAccessVerbPhrase
               ->  FinalizeAccessNotInstrumentF
               -> GameComputation Identity ()
    openit effectKeys searchStrategy actionMap avp finalize = do
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
                Right (ObjectContainerAccessF actionF) -> finalize effectKeys actionF
        CompleteAR (CompleteAccessRes {..}) -> error "openF: Complete Access Result not implemented."
      where
        caRes = parseAccessPhrase avp

lookupAccessAction :: GID Object
                        -> ContainerAccessActionMap
                        -> GameComputation Identity (Either (GameComputation Identity ()) ContainerAccessActionF)
lookupAccessAction objectGID actionMap = do
  actionMgmt <- _objectActionManagement <$> getObjectM objectGID
  case findSAForContainersKey openSA actionMgmt of
    Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence ((Data.Text.pack . show) objectGID <> " does not have a 'get' action.")
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> pure $ Left $ modifyNarration $ updateActionConsequence $ "No acquisition action found for GID: " <> (Data.Text.pack . show) actionGID
        Just action -> do
         pure $ Right action

validateObjectSearch :: SimpleAccessSearchStrategy
                          -> NounKey
                          -> GameComputation Identity (Either AcquisitionError (GID Object))
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing        -> pure $ Left $ ObjectNotFound "You don't see that here."
    Just objectGID -> pure $ Right objectGID
