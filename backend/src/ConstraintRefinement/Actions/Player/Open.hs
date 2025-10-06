module ConstraintRefinement.Actions.Player.Open where
import           Control.Monad.Error.Class                         (throwError)
import           Control.Monad.Identity                            (Identity)
import qualified Data.Map.Strict
import           Data.Text                                         (Text, pack)
import           GameState                                         (getObjectM,
                                                                    modifyNarration,
                                                                    parseAccessPhrase,
                                                                    updateActionConsequence)
import           GameState.ActionManagement                        (findSAForContainersKey,
                                                                    lookupContainerAccessVerbPhrase,
                                                                    processEffectsFromRegistry)
import           GameState.Perception                              (youSeeM)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs (openSA)
import           Model.Actions.Results                             (AccessRes (CompleteAR, SimpleAR),
                                                                    CompleteAccessRes (CompleteAccessRes),
                                                                    SimpleAccessRes (SimpleAccessRes, _saContainerKey))
import           Model.Core                                        (ActionEffectKey,
                                                                    ContainerAccessActionF (CannotAccessF, InstrumentContainerAccessF, ObjectContainerAccessF, PlayerCannotAccessF, PlayerContainerAccessF),
                                                                    ContainerAccessActionMap,
                                                                    FinalizeAccessNotInstrumentF,
                                                                    GameComputation,
                                                                    Object (_objectActionManagement),
                                                                    SimpleAccessSearchStrategy,
                                                                    SomaticAccessActionF (CannotSomaticAccessF, PlayerSomaticAccessActionF))
import           Model.GID                                         (GID)
import           Model.Parser.Composites.Verbs                     (ContainerAccessVerbPhrase)
import           Model.Parser.GCase                                (NounKey)

openDeniedF :: ContainerAccessActionF
openDeniedF = PlayerCannotAccessF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = do
      processEffectsFromRegistry actionEffectKey

openEyesDenied :: SomaticAccessActionF
openEyesDenied = CannotSomaticAccessF denied
 where
   denied :: ActionEffectKey -> GameComputation Identity ()
   denied actionEffectKey = do
     processEffectsFromRegistry actionEffectKey
     modifyNarration $ updateActionConsequence msg
   msg :: Text
   msg = "They're already open, relax."

openEyes :: SomaticAccessActionF
openEyes = PlayerSomaticAccessActionF opened
 where
   opened :: ActionEffectKey
             -> GameComputation Identity ()
   opened actionEffectKeys = do
     processEffectsFromRegistry actionEffectKeys

openF :: ContainerAccessActionF
openF = PlayerContainerAccessF openit
  where
    openit :: ActionEffectKey
               -> SimpleAccessSearchStrategy
               -> ContainerAccessActionMap
               -> ContainerAccessVerbPhrase
               ->  FinalizeAccessNotInstrumentF
               -> GameComputation Identity ()
    openit actionEffectKey searchStrategy actionMap avp finalize = do
      case caRes of
        SimpleAR (SimpleAccessRes {..}) -> do
          objectGID <- validateObjectSearch searchStrategy _saContainerKey
          objectActionLookup <- lookupAccessAction objectGID actionMap
          case objectActionLookup of
            Left errM -> errM
            Right (PlayerCannotAccessF _) -> error $ "Container " ++ show objectGID ++ " has a PlayerCannotAccessF action, which is invalid."
            Right (InstrumentContainerAccessF _) -> error $ "Container " ++ show objectGID ++ " has an InstrumentContainerAccessF action, which is invalid."
            Right (PlayerContainerAccessF _) -> error $ "Container " ++ show objectGID ++ " has a PlayerContainerAccessF action, which is invalid."
            Right (CannotAccessF actionF) -> actionF lookupActionF
            Right (ObjectContainerAccessF actionF) -> finalize actionEffectKey (actionF lookupActionF)
        CompleteAR (CompleteAccessRes {..}) -> error "openF: Complete Access Result not implemented."
      where
        caRes = parseAccessPhrase avp
        lookupActionF = lookupContainerAccessVerbPhrase avp
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
                          -> GameComputation Identity (GID Object)
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing        -> throwError "You don't see that here."
    Just objectGID -> pure objectGID
