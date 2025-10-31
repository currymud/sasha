 module ConstraintRefinement.Actions.Player.Look where
import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad.Except          (MonadError (throwError))
import           Control.Monad.Identity        (Identity)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Maybe                    (listToMaybe)
import qualified Data.Set
import           GameState.ActionManagement    (processEffectsFromRegistry)
import           GameState.Perception          (findAccessibleObject,
                                                queryPerceptionMap)
import           Model.Core                    (AgentDirectionalStimulusActionF (AgentCanLookAtF, AgentCannotLookAtF),
                                                AgentDirectionalStimulusContainerActionF (AgentCanLookInF),
                                                AgentImplicitStimulusActionF (AgentImplicitStimulusActionF),
                                                GameComputation, Object,
                                                SpatialRelationship (ContainedIn, Contains, SupportedBy, Supports))
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Container, DirectionalStimulus)
import           Model.Parser.Composites.Nouns (ContainerPhrase (ContainerPhrase),
                                                DirectionalStimulusNounPhrase (DirectionalStimulusNounPhrase),
                                                NounPhrase (DescriptiveNounPhrase, DescriptiveNounPhraseDet, NounPhrase, SimpleNounPhrase))
import           Model.Parser.GCase            (NounKey (ContainerKey, DirectionalStimulusKey))


findContainer :: Data.Set.Set SpatialRelationship -> Maybe (GID Object)
findContainer relationships =
      listToMaybe [cid | ContainedIn cid <- Data.Set.toList relationships] <|>
      listToMaybe [sid | SupportedBy sid <- Data.Set.toList relationships]

lookInF :: AgentDirectionalStimulusContainerActionF
lookInF = AgentCanLookInF processEffectsFromRegistry

lookAtF :: AgentDirectionalStimulusActionF
lookAtF = AgentCanLookAtF processEffectsFromRegistry

agentLookAtFailF :: AgentDirectionalStimulusActionF
agentLookAtFailF = AgentCannotLookAtF processEffectsFromRegistry

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

agentLookF :: AgentImplicitStimulusActionF
agentLookF = AgentImplicitStimulusActionF processEffectsFromRegistry

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
