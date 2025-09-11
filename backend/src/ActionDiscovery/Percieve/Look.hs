{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
module ActionDiscovery.Percieve.Look (
                              manageImplicitStimulusProcess
                             , manageDirectionalStimulusProcess
                             , manageContainerDirectionalStimulusProcess
                             ) where

import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader.Class    (asks)
import qualified Data.Map.Strict
import qualified Data.Set
import           GameState                     (getLocationM, getPlayerM)
import           GameState.ActionManagement    (lookupDirectionalContainerStimulus,
                                                lookupDirectionalStimulus,
                                                lookupImplicitStimulus)
import           Model.Core                    (ActionMaps (_directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap),
                                                Config (_actionMaps),
                                                DirectionalStimulusActionF (CannotSeeF, ObjectDirectionalStimulusActionF, PlayerDirectionalStimulusActionF),
                                                DirectionalStimulusContainerActionF (CannotSeeInF, PlayerDirectionalStimulusContainerActionF),
                                                ActionEffectKey (ImplicitStimulusActionKey),
                                                EffectKey (ActionKey),
                                                GameComputation,
                                                ImplicitStimulusActionF (ImplicitStimulusActionF),
                                                ImplicitStimulusActionMap,
                                                Player (_location, _playerActions))
import           Model.Parser.Atomics.Verbs    (DirectionalStimulusVerb,
                                                ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns (ContainerPhrase,
                                                DirectionalStimulusNounPhrase)

manageImplicitStimulusProcess :: ImplicitStimulusVerb -> GameComputation Identity ()
manageImplicitStimulusProcess isv = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupImplicitStimulus isv availableActions of
    Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in player "
    Just actionGID -> do
      actionMap :: ImplicitStimulusActionMap <- asks (_implicitStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No implicit stimulus action found for GID: "
        Just (ImplicitStimulusActionF actionFunc) -> do
          let effectKeys = Data.Set.singleton (ActionKey (ImplicitStimulusActionKey actionGID))
          actionFunc effectKeys

manageDirectionalStimulusProcess :: DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ()
manageDirectionalStimulusProcess dsv dsnp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupDirectionalStimulus dsv availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just actionGID -> do
      actionMap <- asks (_directionalStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just (CannotSeeF actionFunc) -> actionFunc
        Just (ObjectDirectionalStimulusActionF _) -> error "Programmer Error: ObjectDirectionalStimulusActionF found in players action map"
        Just (PlayerDirectionalStimulusActionF actionFunc) -> actionFunc dsv dsnp

manageContainerDirectionalStimulusProcess :: DirectionalStimulusVerb -> ContainerPhrase -> GameComputation Identity ()
manageContainerDirectionalStimulusProcess dsv cp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupDirectionalContainerStimulus dsv availableActions of
    Nothing -> error "Programmer Error: No container directional stimulus action found for verb: "
    Just actionGID -> do
      actionMap <- asks (_directionalStimulusContainerActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just (PlayerDirectionalStimulusContainerActionF actionFunc) -> do
          actionFunc dsv cp
        Just (CannotSeeInF actionF)-> actionF
        Just _ -> error "Programmer Error: object action found in players action map"
