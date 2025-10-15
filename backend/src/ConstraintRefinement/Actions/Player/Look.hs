 module ConstraintRefinement.Actions.Player.Look where
import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad.Except          (MonadError (throwError))
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Maybe                    (listToMaybe)
import qualified Data.Set
import           GameState                     (getObjectM, getPlayerLocationM)
import           GameState.ActionManagement    (lookupDirectionalContainerStimulus,
                                                lookupDirectionalStimulus,
                                                lookupImplicitStimulus,
                                                processEffectsFromRegistry)
import           GameState.Perception          (findAccessibleObject,
                                                queryPerceptionMap)
import           Model.Core                    (ActionEffectKey (DirectionalStimulusActionKey, DirectionalStimulusContainerActionKey),
                                                ActionManagementFunctions,
                                                ActionMaps (_directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap),
                                                AgentDirectionalStimulusActionF (AgentCanLookAtF),
                                                Config (_actionMaps),
                                                DirectionalStimulusActionF (ObjectCannotBeSeenF, ObjectDirectionalStimulusActionF, PlayerCannotSeeF, PlayerDirectionalStimulusActionF),
                                                DirectionalStimulusContainerActionF (LocationDirectionalStimulusContainerActionF, ObjectCannotBeSeenInF, ObjectDirectionalStimulusContainerActionF, PlayerCannotSeeInF, PlayerDirectionalStimulusContainerActionF),
                                                GameComputation,
                                                ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF),
                                                Location (_locationActionManagement),
                                                Object (_objectActionManagement),
                                                PlayerDirectionalStimulusAction,
                                                PlayerDirectionalStimulusContainerAction,
                                                SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Container, DirectionalStimulus)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ContainerPhrase (ContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase            (NounKey (ContainerKey, DirectionalStimulusKey))


findContainer :: Data.Set.Set SpatialRelationship -> Maybe (GID Object)
findContainer relationships =
      listToMaybe [cid | ContainedIn cid <- Data.Set.toList relationships] <|>
      listToMaybe [sid | SupportedBy sid <- Data.Set.toList relationships]

lookInF :: DirectionalStimulusContainerActionF
lookInF = PlayerDirectionalStimulusContainerActionF lookInAction
  where
    lookInAction :: PlayerDirectionalStimulusContainerAction
    lookInAction = error "lookInAction undefined"
      {-
    lookInAction actionEffectKey = do
      -- All narration now handled via effects
      -- ToDo
      -- Get Key from Container
      processEffectsFromRegistry actionEffectKey

-}
lookAtF :: DirectionalStimulusActionF
lookAtF = ObjectDirectionalStimulusActionF lookAction
  where
    lookAction actionEffectKey  = do
      -- All narration now handled via effects
      processEffectsFromRegistry actionEffectKey
lookatF' :: AgentDirectionalStimulusActionF
lookatF' = AgentCanLookAtF processEffectsFromRegistry

-- Helper function for supported objects (similar to getContainedObjects)
getSupportedObjects :: GID Object
                    -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                    -> [GID Object]
getSupportedObjects objGID spatialMap =
  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> []
    Just relationships ->
      [oid | Supports oidSet <- Data.Set.toList relationships,
             oid <- Data.Set.toList oidSet]

-- Updated getContainedObjects signature
getContainedObjects :: GID Object
                    -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                    -> [GID Object]
getContainedObjects objGID spatialMap =
  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> []
    Just relationships ->
      [oid | Contains oidSet <- Data.Set.toList relationships,
             oid <- Data.Set.toList oidSet]

isvActionEnabled :: ImplicitStimulusVerb -> ImplicitStimulusActionF
isvActionEnabled isv = PlayerImplicitStimulusActionF actionEnabled
  where
    actionEnabled :: ActionEffectKey -> GameComputation Identity ()
    actionEnabled actionEffectKey = do
      loc <- getPlayerLocationM
      let actionMgmt = _locationActionManagement loc
      case lookupImplicitStimulus isv actionMgmt of
        Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in location map"
        Just actionGID -> do
          actionMap' :: Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in actionmap "
            Just someActionF ->
              let actionF = case someActionF of
                    (PlayerImplicitStimulusActionF actionFunc) -> actionFunc
                    (CannotImplicitStimulusActionF actionFunc) -> actionFunc
              in  actionF actionEffectKey >> processEffectsFromRegistry actionEffectKey

-- Objects don't need look actions , just effects
dsvActionEnabled :: DirectionalStimulusActionF
dsvActionEnabled = PlayerDirectionalStimulusActionF lookit
  where
    lookit :: PlayerDirectionalStimulusAction
    lookit actionEffectKey oid lid lookupActionF = do
      -- 1. Validate player capability and find object
--      oid <- validateObjectLook dsnp
          -- 2. Get object's action management and look up its response
      actionMgmt <- _objectActionManagement <$> getObjectM oid
      case lookupActionF actionMgmt of
        Nothing -> error "Programmer Error: No directional stimulus action found for verb"
        Just actionGID -> do
          let objEffectKey = DirectionalStimulusActionKey actionGID
              -- 3. Get the actual action from the action map
          actionMap <- asks (_directionalStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap of
            Nothing -> error "Programmer Error: No directional stimulus action found for GID"
            Just objectResponse ->
              let objectLookF = case objectResponse of
                    PlayerDirectionalStimulusActionF _ ->
                      error "Programmer Error: PlayerDirectionalStimulusActionF found in object action map"
                    ObjectDirectionalStimulusActionF actionF -> actionF objEffectKey
                    PlayerCannotSeeF _ -> error "Programmer Error: PlayerCannotSeeF found in object action map"
                    ObjectCannotBeSeenF actionF -> actionF objEffectKey
               in  objectLookF  >> processEffectsFromRegistry actionEffectKey
--        lookupActionF = lookupDirectionalStimulus dsv
{-
type PlayerDirectionalStimulusContainerAction :: Type
type PlayerDirectionalStimulusContainerAction
       = ActionEffectKey
           -> GID Object
           -> GID Location
           -> (ActionManagementFunctions -> Maybe (GID DirectionalStimulusContainerActionF))
           -> GameComputation Identity ()
-}
dsvContainerActionEnabled :: DirectionalStimulusContainerActionF
dsvContainerActionEnabled = PlayerDirectionalStimulusContainerActionF lookinit
  where
    lookinit :: PlayerDirectionalStimulusContainerAction
    lookinit actionEffectKey cid lid lookupActionF = do
      -- 1. Validate player capability and find container object
--      containerGID <- validateContainerLook cp
      -- 2. Get container's response action
      actionMgmt <- _objectActionManagement <$> getObjectM cid
      case lookupActionF actionMgmt of
        Nothing -> error "Programmer Error: No directional container stimulus action found for verb"
        Just actionGID -> do
          let containerEffectKey = DirectionalStimulusContainerActionKey actionGID
              -- 3. Get the actual action from the action map
          actionMap <- asks (_directionalStimulusContainerActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap of
            Nothing -> error "Programmer Error: No directional container stimulus action found for GID"
            Just containerResponse ->
              let containerLookF = case containerResponse of
                    ObjectDirectionalStimulusContainerActionF containerLookF' -> containerLookF'
                    ObjectCannotBeSeenInF cannotBeSeenInF -> cannotBeSeenInF
                    PlayerCannotSeeInF _ ->
                      error "Programmer Error: PlayerCannotSeeInF found in object action map"
                    LocationDirectionalStimulusContainerActionF _ ->
                      error "Programmer Error: LocationDirectionalStimulusContainerActionF found in object action map"
                    PlayerDirectionalStimulusContainerActionF _ ->
                      error "Programmer Error: PlayerDirectionalStimulusContainerActionF found in object action map"
              in containerLookF containerEffectKey >> processEffectsFromRegistry actionEffectKey

extractContainerNoun :: ContainerPhrase -> NounKey
extractContainerNoun (ContainerPhrase nounPhrase) = extractContainerNounFromPhrase nounPhrase
  where
    extractContainerNounFromPhrase :: NounPhrase Container -> NounKey
    extractContainerNounFromPhrase (SimpleNounPhrase container) = ContainerKey container
    extractContainerNounFromPhrase (NounPhrase _ container) = ContainerKey container
    extractContainerNounFromPhrase (DescriptiveNounPhrase _ container) = ContainerKey container
    extractContainerNounFromPhrase (DescriptiveNounPhraseDet _ _ container) = ContainerKey container

-- Equivalent to validateObjectSearch but for containers
validateContainerLook :: ContainerPhrase
                      -> GameComputation Identity (GID Object)
validateContainerLook cp = do
  let containerNounKey = extractContainerNoun cp
  findAccessibleObject containerNounKey >>= \case
    Just containerGID -> pure containerGID
    Nothing -> throwError "That is not here."

validateObjectLook :: DirectionalStimulusNounPhrase
                   -> GameComputation Identity (GID Object)
validateObjectLook dsnp = do
  -- Try direct perception map first
  queryPerceptionMap dsnp >>= \case
    objGIDSet | not (Data.Set.null objGIDSet) -> do
      let firstObjGID = Data.Set.elemAt 0 objGIDSet
      pure firstObjGID
    _ -> do
      -- Try accessible objects
      let noun = extractNoun dsnp
      findAccessibleObject (DirectionalStimulusKey noun) >>= \case
        Just objGID -> pure objGID
        Nothing -> throwError "That's not here. Try something else."

extractNoun :: DirectionalStimulusNounPhrase -> DirectionalStimulus
extractNoun (DirectionalStimulusNounPhrase _ np) = case np of
  SimpleNounPhrase noun             -> noun
  NounPhrase _ noun                 -> noun
  DescriptiveNounPhrase _ noun      -> noun
  DescriptiveNounPhraseDet _ _ noun -> noun
