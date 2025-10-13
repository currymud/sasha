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
import           GameState                     (getObjectM,
                                                getPlayerLocationGID,
                                                getPlayerM)
import           GameState.ActionManagement    (lookupDirectionalContainerStimulus,
                                                lookupDirectionalStimulus,
                                                lookupImplicitStimulus)
import           GameState.Perception          (findAccessibleObject,
                                                queryPerceptionMap)
import           Model.Core                    (ActionEffectKey (DirectionalStimulusActionKey, DirectionalStimulusContainerActionKey, ImplicitStimulusActionKey),
                                                ActionMaps (_directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap),
                                                Config (_actionMaps),
                                                DirectionalStimulusActionF (ObjectCannotBeSeenF, ObjectDirectionalStimulusActionF, PlayerCannotSeeF, PlayerDirectionalStimulusActionF),
                                                DirectionalStimulusContainerActionF (PlayerCannotSeeInF, PlayerDirectionalStimulusContainerActionF),
                                                GameComputation,
                                                ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF),
                                                ImplicitStimulusActionMap,
                                                Object (_objectActionManagement),
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

manageDirectionalStimulusProcess :: DirectionalStimulusVerb
                                      -> DirectionalStimulusNounPhrase
                                      -> GameComputation Identity ()
manageDirectionalStimulusProcess dsv dsnp = do
  availableActions <- _playerActions <$> getPlayerM
  case lookupActionF availableActions of
    Nothing -> error "Programmer Error: No directional stimulus action found for verb: "
    Just actionGID -> do
      let actionEffectKey = DirectionalStimulusActionKey actionGID
      actionMap <- asks (_directionalStimulusActionMap . _actionMaps)
      case Data.Map.Strict.lookup actionGID actionMap of
        Nothing -> error "Programmer Error: No directional stimulus action found for GID: "
        Just (ObjectDirectionalStimulusActionF _) ->
          error "Programmer Error: ObjectDirectionalStimulusActionF found in players action map"
        Just (ObjectCannotBeSeenF _) ->
          error "Programmer Error: ObjectCannotBeSeenF found in players action map"
        Just (PlayerCannotSeeF actionFunc) -> actionFunc actionEffectKey
        Just (PlayerDirectionalStimulusActionF actionFunc) -> do
          oid <- validateObjectLook dsnp
          lid <- getPlayerLocationGID
          actionFunc actionEffectKey oid lid lookupActionF
  where
    lookupActionF = lookupDirectionalStimulus dsv
{-
 type PlayerDirectionalStimulusAction :: Type
type PlayerDirectionalStimulusAction
  = ActionEffectKey
      -> GID Object
      -> GID Location
      -> (ActionManagementFunctions -> Maybe (GID DirectionalStimulusActionF))
      -> GameComputation Identity ()
-}
--      containerGID <- validateContainerLook cp
            {-
type DirectionalStimulusActionF :: Type
data DirectionalStimulusActionF
  = PlayerDirectionalStimulusActionF (ActionEffectKey -> DirectionalStimulusVerb -> DirectionalStimulusNounPhrase -> GameComputation Identity ())
  | ObjectDirectionalStimulusActionF (ActionEffectKey -> GameComputation Identity ())
  | PlayerCannotSeeF (ActionEffectKey -> (GameComputation Identity ()))
  | ObjectCannotBeSeenF (ActionEffectKey -> GameComputation Identity ())

type PlayerDirectionalStimulusContainerAction :: Type
type PlayerDirectionalStimulusContainerAction
       = ActionEffectKey
           -> GID Object
           -> GID Location
           -> (ActionManagementFunctions -> Maybe (GID DirectionalStimulusContainerActionF))
           -> GameComputation Identity ()
-}
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
