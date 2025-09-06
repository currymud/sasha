{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module Build.BedPuzzle.Actions.Player.Look (
                              dsvActionEnabled
                             , dsvContainerActionEnabled
                             , isvActionEnabled
                             ) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (getObjectM, modifyNarration)
import           GameState.ActionManagement    (lookupDirectionalContainerStimulus,
                                                lookupDirectionalStimulus,
                                                lookupImplicitStimulus)
import           GameState.Perception          (findAccessibleObject,
                                                queryPerceptionMap)
import           Model.GameState               (ActionMaps (_directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap),
                                                Config (_actionMaps),
                                                DirectionalStimulusActionF (CannotSeeF, ObjectDirectionalStimulusActionF, PlayerDirectionalStimulusActionF),
                                                DirectionalStimulusContainerActionF (CannotSeeInF, ObjectDirectionalStimulusContainerActionF, PlayerDirectionalStimulusContainerActionF),
                                                GameComputation,
                                                ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                Location (_locationActionManagement),
                                                Object (_objectActionManagement),
                                                updateActionConsequence)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Container, DirectionalStimulus)
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ContainerPhrase (ContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase            (NounKey (ContainerKey, DirectionalStimulusKey))

isvActionEnabled :: ImplicitStimulusVerb -> ImplicitStimulusActionF
isvActionEnabled isv = ImplicitStimulusActionF actionEnabled
  where
    actionEnabled player loc = do
      let actionMgmt = _locationActionManagement loc
      case lookupImplicitStimulus isv actionMgmt of
        Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in location map"
        Just actionGID -> do
          actionMap' :: Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in actionmap "
            Just (ImplicitStimulusActionF actionFunc) -> actionFunc player loc

-- since we only look at or look in, we don't need to pass the verb to the object action
-- ToDo fix it
dsvActionEnabled :: DirectionalStimulusActionF
dsvActionEnabled = PlayerDirectionalStimulusActionF lookit
  where
    lookit :: DirectionalStimulusVerb
           -> DirectionalStimulusNounPhrase
           -> GameComputation Identity ()
    lookit dsv dsnp = do
      -- 1. Validate player capability and find object
      objectValidation <- validateObjectLook dsnp
      case objectValidation of
        Left lookError -> modifyNarration $ updateActionConsequence lookError
        Right objectGID -> do
          -- 2. Get object's action management and look up its response
          actionMgmt <- _objectActionManagement <$> getObjectM objectGID
          case lookupDirectionalStimulus dsv actionMgmt of
            Nothing -> error "Programmer Error: No directional stimulus action found for verb"
            Just actionGID -> do
              -- 3. Get the actual action from the action map
              actionMap <- asks (_directionalStimulusActionMap . _actionMaps)
              case Data.Map.Strict.lookup actionGID actionMap of
                Nothing -> error "Programmer Error: No directional stimulus action found for GID"
                Just objectResponse -> case objectResponse of
                  -- 4. Pattern match on object's response constructor
                  ObjectDirectionalStimulusActionF objectLookF -> objectLookF
                  CannotSeeF cannotSeeF -> cannotSeeF
                  PlayerDirectionalStimulusActionF _ ->
                    error "Programmer Error: PlayerDirectionalStimulusActionF found in object action map"

dsvContainerActionEnabled :: DirectionalStimulusContainerActionF
dsvContainerActionEnabled = PlayerDirectionalStimulusContainerActionF lookinit
  where
    lookinit :: DirectionalStimulusVerb
             -> ContainerPhrase
             -> GameComputation Identity ()
    lookinit dsv cp = do
      -- 1. Validate player capability and find container object
      containerValidation <- validateContainerLook cp
      case containerValidation of
        Left lookError ->  modifyNarration $ updateActionConsequence lookError
        Right containerGID -> do
          -- 2. Get container's response action
          actionMgmt <- _objectActionManagement <$> getObjectM containerGID
          case lookupDirectionalContainerStimulus dsv actionMgmt of
            Nothing -> error "Programmer Error: No directional container stimulus action found for verb"
            Just actionGID -> do
              -- 3. Get the actual action from the action map
              actionMap <- asks (_directionalStimulusContainerActionMap . _actionMaps)
              case Data.Map.Strict.lookup actionGID actionMap of
                Nothing -> error "Programmer Error: No directional container stimulus action found for GID"
                Just containerResponse -> case containerResponse of
                  -- 4. Pattern match on container's response constructor
                  ObjectDirectionalStimulusContainerActionF containerLookF -> containerLookF
                  CannotSeeInF cannotSeeInF -> cannotSeeInF
                  PlayerDirectionalStimulusContainerActionF _ ->
                    error "Programmer Error: PlayerDirectionalStimulusContainerActionF found in object action map"

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
                      -> GameComputation Identity (Either Text (GID Object))
validateContainerLook cp = do
  let containerNounKey = extractContainerNoun cp
  findAccessibleObject containerNounKey >>= \case
    Just containerGID -> pure $ Right containerGID
    Nothing -> pure $ Left "That is not here."

validateObjectLook :: DirectionalStimulusNounPhrase
                   -> GameComputation Identity (Either Text (GID Object))
validateObjectLook dsnp = do
  -- Try direct perception map first
  queryPerceptionMap dsnp >>= \case
    objGIDSet | not (Data.Set.null objGIDSet) -> do
      let firstObjGID = Data.Set.elemAt 0 objGIDSet
      pure $ Right firstObjGID
    _ -> do
      -- Try accessible objects
      let noun = extractNoun dsnp
      findAccessibleObject (DirectionalStimulusKey noun) >>= \case
        Just objGID -> pure $ Right objGID
        Nothing -> pure $ Left "That's not here. Try something else."

extractNoun :: DirectionalStimulusNounPhrase -> DirectionalStimulus
extractNoun (DirectionalStimulusNounPhrase _ np) = case np of
  SimpleNounPhrase noun             -> noun
  NounPhrase _ noun                 -> noun
  DescriptiveNounPhrase _ noun      -> noun
  DescriptiveNounPhraseDet _ _ noun -> noun
