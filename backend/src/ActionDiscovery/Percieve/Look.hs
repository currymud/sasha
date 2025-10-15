{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module ActionDiscovery.Percieve.Look (
                              manageImplicitStimulusProcess
                             , manageDirectionalStimulusProcess
                             , manageContainerDirectionalStimulusProcess
                             ) where

import           Control.Monad.Except          (MonadError (throwError))
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationM, getObjectM,
                                                getPlayerLocationGID,
                                                getPlayerM)
import           GameState.ActionManagement    (lookupAgentDirectionalStimulus,
                                                lookupAgentImplicitStimulus,
                                                lookupDirectionalContainerStimulus,
                                                lookupDirectionalStimulus,
                                                lookupImplicitStimulus,
                                                lookupLocationDirectionalStimulus,
                                                lookupLocationImplicitStimulus,
                                                lookupObjectDirectionalStimulus)
import           GameState.Perception          (findAccessibleObject,
                                                queryPerceptionMap)
import           Model.Core                    (ActionEffectKey (AgentDirectionalStimulusActionKey, AgentImplicitStimulusActionKey, DirectionalStimulusActionKey, DirectionalStimulusContainerActionKey, ImplicitStimulusActionKey, LocationDirectionalStimulusActionKey, LocationImplicitStimulusActionKey, ObjectDirectionalStimulusActionKey),
                                                ActionMaps (_agentDirectionalStimulusActionMap, _agentImplicitStimulusActionMap, _directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap, _locationDirectionalStimulusActionMap, _locationImplicitStimulusActionMap, _objectDirectionalStimulusActionMap),
                                                AgentDirectionalStimulusActionF (AgentCanLookAtF, AgentCannotLookAtF),
                                                AgentImplicitStimulusActionF (AgentCanSeeF, AgentCannotSeeF),
                                                Config (_actionMaps),
                                                DirectionalStimulusActionF (ObjectCannotBeSeenF, ObjectDirectionalStimulusActionF, PlayerCannotSeeF, PlayerDirectionalStimulusActionF),
                                                DirectionalStimulusContainerActionF (PlayerCannotSeeInF, PlayerDirectionalStimulusContainerActionF),
                                                GameComputation,
                                                ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF),
                                                ImplicitStimulusActionMap,
                                                Location (_locationActionManagement),
                                                LocationDirectionalStimulusActionF (LocationCanBeSeenF, LocationCannotBeSeenF),
                                                LocationImplicitStimulusActionF (LocationCanBeSeenImplicitF, LocationCannotBeSeenImplicitF),
                                                Object (_objectActionManagement),
                                                ObjectDirectionalStimulusActionF (ObjectCanBeSeenF, ObjectCannotBeSeenF'),
                                                Player (_playerActions))
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Container, DirectionalStimulus)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ContainerPhrase (ContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase            (NounKey (ContainerKey, DirectionalStimulusKey))

manageImplicitStimulusProcess :: ImplicitStimulusVerb
                                   -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupImplicitStimulus isv availableActions of
    Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in player "
    Just actionGID -> do
      actionMap :: ImplicitStimulusActionMap <- asks (_implicitStimulusActionMap . _actionMaps)
      let actionEffectKey = ImplicitStimulusActionKey actionGID
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No implicit stimulus action found for GID: "
        Just (PlayerImplicitStimulusActionF actionFunc) -> actionFunc actionEffectKey
        Just (CannotImplicitStimulusActionF actionFunc) -> actionFunc actionEffectKey

manageImplicitStimulusProcess' :: ImplicitStimulusVerb
                                    -> GameComputation Identity ()
manageImplicitStimulusProcess' isv = do
  availableAgentActions <- _playerActions <$> getPlayerM
  locationM <-  getLocationM <$> getPlayerLocationGID
  availableLocationActions <- _locationActionManagement <$> locationM
  let lookupAgentAction = lookupAgentActionF availableAgentActions
      lookupLocationAction = lookupLocationActionF availableLocationActions
  case (lookupAgentAction, lookupLocationAction) of
    (Nothing,_) -> error "Programmer Error: No agent implicit stimulus action found for verb: "
    (_,Nothing) -> error "Programmer Error: No location implicit stimulus action found for verb: "
    (Just agentActionGID, Just locationActionGID) -> do
      let agentActionEffectKey = AgentImplicitStimulusActionKey agentActionGID
          locationActionEffectKey = LocationImplicitStimulusActionKey locationActionGID
      agentActionMap <- asks (_agentImplicitStimulusActionMap . _actionMaps)
      locationActionMap <- asks (_locationImplicitStimulusActionMap . _actionMaps)
      agentAction <- maybe (error "Programmer Error: No agent action found for GID") pure
                     (Data.Map.Strict.lookup agentActionGID agentActionMap)
      locationAction <- maybe (error "Programmer Error: No location action found for GID") pure
                        (Data.Map.Strict.lookup locationActionGID locationActionMap)
      case (agentAction, locationAction) of
        (AgentCannotSeeF actionF, _) -> actionF agentActionEffectKey
        (_, LocationCannotBeSeenImplicitF locationActionF) -> locationActionF locationActionEffectKey
        (AgentCanSeeF agentActionF, LocationCanBeSeenImplicitF locationActionF) ->
          agentActionF agentActionEffectKey >> locationActionF locationActionEffectKey
      pure ()
  where
    lookupAgentActionF = lookupAgentImplicitStimulus isv
    lookupLocationActionF = lookupLocationImplicitStimulus isv
      {-
type AgentImplicitStimulusActionF :: Type
data AgentImplicitStimulusActionF
  = AgentCanSeeF (ActionEffectKey -> GameComputation Identity ())
  | AgentCannotSeeF (ActionEffectKey -> GameComputation Identity ())

type LocationImplicitStimulusActionF :: Type
data LocationImplicitStimulusActionF
  = LocationCanBeSeenImplicitF (ActionEffectKey -> GameComputation Identity ())
  | LocationCannotBeSeenImplicitF (ActionEffectKey -> GameComputation Identity ())
-}
manageDirectionalStimulusProcess :: DirectionalStimulusVerb
                                      -> DirectionalStimulusNounPhrase
                                      -> GameComputation Identity ()
manageDirectionalStimulusProcess dsv dsnp = do
  availableAgentActions <- _playerActions <$> getPlayerM
  objM <- getObjectM <$> validateObjectLook dsnp
  availableObjectActions <- _objectActionManagement <$> objM
  locM <- getLocationM <$> getPlayerLocationGID
  availableLocationActions <- _locationActionManagement <$> locM
  let lookupAgentAction = lookupAgentActionF availableAgentActions
      lookupObjectAction = lookupObjectActionF availableObjectActions
      lookupLocationAction = lookupLocationActionF availableLocationActions
  case (lookupAgentAction, lookupLocationAction, lookupObjectAction) of
    (Nothing,_,_) -> error "Programmer Error: No agent directional stimulus action found for verb: "
    (_,Nothing,_) -> error "Programmer Error: No location directional stimulus action found for verb: "
    (_,_,Nothing) -> error "Programmer Error: No object directional stimulus action found for verb: "

    (Just agentActionGID, Just locationActionGID, Just objectActionGID) -> do
      let agentActionEffectKey = AgentDirectionalStimulusActionKey agentActionGID
          locationActionEffectKey = LocationDirectionalStimulusActionKey locationActionGID
          objectActionEffectKey = ObjectDirectionalStimulusActionKey objectActionGID
      agentActionMap <- asks (_agentDirectionalStimulusActionMap . _actionMaps)
      locationActionMap <- asks (_locationDirectionalStimulusActionMap . _actionMaps)
      objectActionMap <- asks (_objectDirectionalStimulusActionMap . _actionMaps)

      agentAction <- maybe (error "Programmer Error: No agent action found for GID") pure
                     (Data.Map.Strict.lookup agentActionGID agentActionMap)
      locationAction <- maybe (error "Programmer Error: No location action found for GID") pure
                        (Data.Map.Strict.lookup locationActionGID locationActionMap)
      objectAction <- maybe (error "Programmer Error: No object action found for GID") pure
                      (Data.Map.Strict.lookup objectActionGID objectActionMap)

      case (agentAction, locationAction, objectAction) of
        (AgentCannotLookAtF actionF, _, _) -> actionF agentActionEffectKey
        (AgentCanLookAtF _, LocationCannotBeSeenF locationActionF, _) ->
          locationActionF locationActionEffectKey
        (AgentCanLookAtF agentActionF, LocationCanBeSeenF _, ObjectCannotBeSeenF' objectActionF) ->
          agentActionF agentActionEffectKey >> objectActionF objectActionEffectKey
        (AgentCanLookAtF agentActionF, LocationCanBeSeenF locationActionF, ObjectCanBeSeenF objectActionF) ->
          agentActionF agentActionEffectKey >> locationActionF locationActionEffectKey >> objectActionF objectActionEffectKey
  where
    lookupAgentActionF = lookupAgentDirectionalStimulus dsv
    lookupLocationActionF = lookupLocationDirectionalStimulus dsv
    lookupObjectActionF = lookupObjectDirectionalStimulus dsv

manageContainerDirectionalStimulusProcess :: DirectionalStimulusVerb
                                               -> ContainerPhrase
                                               -> GameComputation Identity ()
manageContainerDirectionalStimulusProcess dsv cp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupActionF availableActions of
    Nothing -> error "Programmer Error: No container directional stimulus action found for verb: "
    Just actionGID -> do
      let actionEffectKey = DirectionalStimulusContainerActionKey actionGID
      actionMap <- asks (_directionalStimulusContainerActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just (PlayerDirectionalStimulusContainerActionF actionFunc) -> do
          cid <- validateContainerLook cp
          lid <- getPlayerLocationGID
          actionFunc actionEffectKey cid lid lookupActionF
        Just (PlayerCannotSeeInF actionF)-> actionF actionEffectKey
        Just _ -> error "Programmer Error: object action found in players action map"
  where
    lookupActionF = lookupDirectionalContainerStimulus dsv

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

extractContainerNoun :: ContainerPhrase -> NounKey
extractContainerNoun (ContainerPhrase nounPhrase) = extractContainerNounFromPhrase nounPhrase
  where
    extractContainerNounFromPhrase :: NounPhrase Container -> NounKey
    extractContainerNounFromPhrase (SimpleNounPhrase container) = ContainerKey container
    extractContainerNounFromPhrase (NounPhrase _ container) = ContainerKey container
    extractContainerNounFromPhrase (DescriptiveNounPhrase _ container) = ContainerKey container
    extractContainerNounFromPhrase (DescriptiveNounPhraseDet _ _ container) = ContainerKey container

extractNoun :: DirectionalStimulusNounPhrase -> DirectionalStimulus
extractNoun (DirectionalStimulusNounPhrase _ np) = case np of
  SimpleNounPhrase noun             -> noun
  NounPhrase _ noun                 -> noun
  DescriptiveNounPhrase _ noun      -> noun
  DescriptiveNounPhraseDet _ _ noun -> noun
