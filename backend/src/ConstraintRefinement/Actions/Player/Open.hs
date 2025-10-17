module ConstraintRefinement.Actions.Player.Open where
import           Control.Monad.Error.Class                         (throwError)
import           Control.Monad.Identity                            (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                                         (pack)
import           GameState                                         (getObjectM,
                                                                    getPlayerLocationM,
                                                                    parseContainerAccessPhrase)
import           GameState.ActionManagement                        (findSAForContainersKey,
                                                                    lookupContainerAccessVerbPhrase,
                                                                    processEffectsFromRegistry)
import           Grammar.Parser.Partitions.Verbs.SimpleAccessVerbs (openSA)
import           Model.Core                                        (AccessRes (CompleteAR, SimpleAR),
                                                                    ActionEffectKey (ContainerAccessActionKey),
                                                                    ActionManagementFunctions,
                                                                    CompleteAccessRes (CompleteAccessRes),
                                                                    ContainerAccessActionF (CannotAccessF, InstrumentContainerAccessF, ObjectContainerAccessF, PlayerCannotAccessF, PlayerContainerAccessF),
                                                                    ContainerAccessActionMap,
                                                                    GameComputation,
                                                                    Location (_objectSemanticMap),
                                                                    Object (_objectActionManagement),
                                                                    SimpleAccessRes (SimpleAccessRes, _saContainerKey),
                                                                    SimpleAccessSearchStrategy,
                                                                    SomaticAccessActionF (CannotSomaticAccessF, PlayerSomaticAccessActionF))
import           Model.GID                                         (GID)
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

openEyes :: SomaticAccessActionF
openEyes = PlayerSomaticAccessActionF opened
 where
   opened :: ActionEffectKey
             -> GameComputation Identity ()
   opened actionEffectKey = do
     processEffectsFromRegistry actionEffectKey

openF :: ContainerAccessActionF
openF = PlayerContainerAccessF openit
  where
    openit :: ActionEffectKey
               -> AccessRes
               -> ContainerAccessActionMap
               -> (ActionManagementFunctions -> Maybe (GID ContainerAccessActionF))
               -> GameComputation Identity ()
    openit playerActionEffectKey caRes actionMap lookupActionF = do
      case caRes of
        SimpleAR (SimpleAccessRes {..}) -> do
          oid <- validateObjectSearch objectSearchStrategy _saContainerKey
          objectActionLookup <- lookupAccessAction oid actionMap
          case objectActionLookup of
            (PlayerCannotAccessF _) -> error $ "Container " ++ show oid ++ " has a PlayerCannotAccessF action, which is invalid."
            (InstrumentContainerAccessF _) -> error $ "Container " ++ show oid ++ " has an InstrumentContainerAccessF action, which is invalid."
            (PlayerContainerAccessF _) -> error $ "Container " ++ show oid ++ " has a PlayerContainerAccessF action, which is invalid."
            actionF' -> do
              actionManagement <- _objectActionManagement <$> getObjectM oid
              let objectEffectF = case lookupActionF actionManagement of
                    Nothing -> throwError $ "You can't open the " <> pack (show _saContainerKey) <> "."
                    Just aid  ->
                      let objectActionKey = ContainerAccessActionKey aid
                      in case actionF' of
                        (CannotAccessF actionF) -> actionF objectActionKey
                        (ObjectContainerAccessF actionF) -> actionF objectActionKey
              processEffectsFromRegistry playerActionEffectKey >> objectEffectF
        CompleteAR (CompleteAccessRes {..}) -> error "openF: Complete Access Result not implemented."

objectSearchStrategy :: SimpleAccessSearchStrategy
objectSearchStrategy nounkey = do
  objectSemanticMap <- _objectSemanticMap <$> getPlayerLocationM
  case Data.Map.Strict.lookup nounkey objectSemanticMap of
    Just objSet
      | not (Data.Set.null objSet) -> pure $ Just (Data.Set.elemAt 0 objSet)
    _ -> pure Nothing

lookupAccessAction :: GID Object
                        -> ContainerAccessActionMap
                        -> GameComputation Identity ContainerAccessActionF
lookupAccessAction oid actionMap = do
  actionMgmt <- _objectActionManagement <$> getObjectM oid
  case findSAForContainersKey openSA actionMgmt of
    Nothing -> error ("Programmer error: " <> show oid <> " does not have a 'get' action.")
    Just actionGID -> do
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error ("Programmer Error: No acquisition action found for GID: " <> show actionGID)
        Just action -> pure action

validateObjectSearch :: SimpleAccessSearchStrategy
                          -> NounKey
                          -> GameComputation Identity (GID Object)
validateObjectSearch searchStrategy nounKey = do
  maybeResult <- searchStrategy nounKey
  case maybeResult of
    Nothing        -> throwError "You don't see that here."
    Just objectGID -> pure objectGID
