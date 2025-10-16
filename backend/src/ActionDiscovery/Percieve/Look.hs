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
import           GameState.ActionManagement    (lookupAgentDirectionalContainerStimulus,
                                                lookupAgentDirectionalStimulus,
                                                lookupAgentImplicitStimulus,
                                                lookupContainerDirectionalContainerStimulus,
                                                lookupDirectionalContainerStimulus,
                                                lookupLocationDirectionalContainerStimulus,
                                                lookupLocationDirectionalStimulus,
                                                lookupLocationImplicitStimulus,
                                                lookupObjectDirectionalStimulus)
import           GameState.Perception          (findAccessibleObject,
                                                queryPerceptionMap)
import           Model.Core                    (ActionEffectKey (AgentDirectionalStimulusActionKey, AgentDirectionalStimulusContainerActionKey, AgentImplicitStimulusActionKey, ContainerDirectionalStimulusContainerActionKey, DirectionalStimulusContainerActionKey, LocationDirectionalStimulusActionKey, LocationDirectionalStimulusContainerActionKey, LocationImplicitStimulusActionKey, ObjectDirectionalStimulusActionKey),
                                                ActionMaps (_agentDirectionalStimulusActionMap, _agentDirectionalStimulusContainerActionMap, _agentImplicitStimulusActionMap, _containerDirectionalStimulusContainerActionMap, _directionalStimulusContainerActionMap, _locationDirectionalStimulusActionMap, _locationDirectionalStimulusContainerActionMap, _locationImplicitStimulusActionMap, _objectDirectionalStimulusActionMap),
                                                AgentDirectionalStimulusActionF (AgentCanLookAtF, AgentCannotLookAtF),
                                                AgentDirectionalStimulusContainerActionF (AgentCanLookInF, AgentCannotLookInF),
                                                AgentImplicitStimulusActionF (AgentCanSeeF, AgentCannotSeeF),
                                                Config (_actionMaps),
                                                ContainerDirectionalStimulusContainerActionF (ContainerCanBeSeenInF, ContainerCannotBeSeenInF'),
                                                DirectionalStimulusContainerActionF (PlayerCannotSeeInF, PlayerDirectionalStimulusContainerActionF),
                                                GameComputation,
                                                Location (_locationActionManagement),
                                                LocationDirectionalStimulusActionF (LocationCanBeSeenF, LocationCannotBeSeenF),
                                                LocationDirectionalStimulusContainerActionF (LocationCanBeSeenInF, LocationCannotBeSeenInF),
                                                LocationImplicitStimulusActionF (LocationCanBeSeenImplicitF, LocationCannotBeSeenImplicitF),
                                                Object (_objectActionManagement),
                                                ObjectDirectionalStimulusActionF (ObjectCanBeSeenF, ObjectCannotBeSeenF'),
                                                Player (_playerActions))
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Container, DirectionalStimulus)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ContainerPhrase (ContainerPhrase),
                                                DirectionalStimulusContainerPhrase (DirectionalStimulusContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase            (NounKey (ContainerKey, DirectionalStimulusKey))

manageImplicitStimulusProcess :: ImplicitStimulusVerb
                                    -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
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
        (AgentCannotLookAtF actionF, _, _) ->
          actionF agentActionEffectKey
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
                                                -> DirectionalStimulusContainerPhrase
                                                -> GameComputation Identity ()
manageContainerDirectionalStimulusProcess dsv (DirectionalStimulusContainerPhrase _ cp) = do
  availableAgentActions <- _playerActions <$> getPlayerM
  objM <- getObjectM <$> validateContainerLook cp
  availableObjectActions <- _objectActionManagement <$> objM
  locM <- getLocationM <$> getPlayerLocationGID
  availableLocationActions <- _locationActionManagement <$> locM
  let lookupAgentAction = lookupAgentActionF availableAgentActions
      lookupObjectAction = lookupObjectActionF availableObjectActions
      lookupLocationAction = lookupLocationActionF availableLocationActions
  case (lookupAgentAction, lookupLocationAction, lookupObjectAction) of
    (Nothing,_,_) -> error "Programmer Error: No agent directional stimulus container action found for verb: "
    (_,Nothing,_) -> error "Programmer Error: No location directional stimulus container action found for verb: "
    (_,_,Nothing) -> error "Programmer Error: No object directional stimulus container action found for verb: "

    (Just agentActionGID, Just locationActionGID, Just containerActionGID) -> do
      let agentActionEffectKey = AgentDirectionalStimulusContainerActionKey agentActionGID
          locationActionEffectKey = LocationDirectionalStimulusContainerActionKey locationActionGID
          containerActionEffectKey = ContainerDirectionalStimulusContainerActionKey containerActionGID
      agentActionMap <- asks (_agentDirectionalStimulusContainerActionMap . _actionMaps)
      locationActionMap <- asks (_locationDirectionalStimulusContainerActionMap . _actionMaps)
      containerActionMap <- asks (_containerDirectionalStimulusContainerActionMap . _actionMaps)

      agentAction <- maybe (error "Programmer Error: No agent action found for GID") pure
                     (Data.Map.Strict.lookup agentActionGID agentActionMap)
      locationAction <- maybe (error "Programmer Error: No location action found for GID") pure
                        (Data.Map.Strict.lookup locationActionGID locationActionMap)
      containerAction <- maybe (error "Programmer Error: No object action found for GID") pure
                      (Data.Map.Strict.lookup containerActionGID containerActionMap)

      case (agentAction, locationAction, containerAction) of
        (AgentCannotLookInF actionF, _, _) ->
          actionF agentActionEffectKey
        (AgentCanLookInF _, LocationCannotBeSeenInF locationActionF, _) ->
          locationActionF locationActionEffectKey
        (AgentCanLookInF agentActionF, LocationCanBeSeenInF _, ContainerCannotBeSeenInF' objectActionF) ->
          agentActionF agentActionEffectKey >> objectActionF containerActionEffectKey
        (AgentCanLookInF agentActionF, LocationCanBeSeenInF locationActionF, ContainerCanBeSeenInF objectActionF) ->
          agentActionF agentActionEffectKey >> locationActionF locationActionEffectKey >> objectActionF containerActionEffectKey
  where
    lookupAgentActionF = lookupAgentDirectionalContainerStimulus dsv
    lookupLocationActionF = lookupLocationDirectionalContainerStimulus dsv
    lookupObjectActionF = lookupContainerDirectionalContainerStimulus dsv

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
