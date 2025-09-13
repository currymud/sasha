 module ConstraintRefinement.Actions.Player.Look where
import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad                 (filterM, unless)
import           Control.Monad.Identity        (Identity)
import           Control.Monad.Reader          (asks)
import           Control.Monad.State           (gets)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict
import           Data.Maybe                    (listToMaybe)
import qualified Data.Set
import           Data.Text                     (Text, intercalate)
import           GameState                     (getObjectM, getPlayerLocationM,
                                                getPlayerM, modifyNarration,
                                                updateActionConsequence)
import           GameState.ActionManagement    (lookupDirectionalContainerStimulus,
                                                lookupDirectionalStimulus,
                                                lookupImplicitStimulus)
import           GameState.Perception          (findAccessibleObject,
                                                isObjectPerceivable,
                                                queryPerceptionMap)
import           Model.Core                    (ActionEffectKey (ImplicitStimulusActionKey),
                                                ActionMaps (_directionalStimulusActionMap, _directionalStimulusContainerActionMap, _implicitStimulusActionMap),
                                                Config (_actionMaps),
                                                DirectionalStimulusActionF (CannotSeeF, ObjectDirectionalStimulusActionF, PlayerDirectionalStimulusActionF),
                                                DirectionalStimulusContainerActionF (CannotSeeInF, ObjectDirectionalStimulusContainerActionF, PlayerDirectionalStimulusContainerActionF),
                                                GameComputation,
                                                GameState (_world),
                                                ImplicitStimulusActionF (CannotImplicitStimulusActionF, PlayerImplicitStimulusActionF),
                                                ImplicitStimulusActionMap,
                                                Location (_locationActionManagement),
                                                Object (_description, _objectActionManagement, _shortName),
                                                SpatialRelationship (ContainedIn, Contains, Inventory, SupportedBy, Supports),
                                                SpatialRelationshipMap (SpatialRelationshipMap),
                                                World (_spatialRelationshipMap))
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

lookInF :: GID Object -> Text -> DirectionalStimulusContainerActionF
lookInF containerGID flavorText = ObjectDirectionalStimulusContainerActionF lookInAction
  where
    lookInAction :: GameComputation Identity ()
    lookInAction = do
      -- First add the flavor text
      modifyNarration $ updateActionConsequence flavorText

      -- Get spatial relationships once
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

      -- Pass spatialMap to avoid duplicate lookup
      let containedObjects = getContainedObjects containerGID spatialMap

      if null containedObjects
        then modifyNarration $ updateActionConsequence "It's empty."
        else do
          -- Get descriptions of contained objects
          descriptions <- mapM getObjectDescription containedObjects
          let contentText = "You see: " <> Data.Text.intercalate ", " descriptions
          modifyNarration $ updateActionConsequence contentText

    getObjectDescription objGID = do
      obj <- getObjectM objGID
      pure $ _shortName obj

lookAtF :: GID Object -> DirectionalStimulusActionF
lookAtF objGID = ObjectDirectionalStimulusActionF lookAction
  where
    lookAction = do
      -- Get object info
      obj <- getObjectM objGID
      -- Get spatial relationships
      world <- gets _world
      let SpatialRelationshipMap spatialMap = _spatialRelationshipMap world

      -- Let generateLocationNarration handle the case deconstruction
      generateLocationNarration obj objGID spatialMap
      -- Also check what's on/in this object
      generateContentsNarration objGID spatialMap

-- Updated to handle the case deconstruction internally
generateLocationNarration :: Object
                          -> GID Object
                          -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                          -> GameComputation Identity ()
generateLocationNarration obj objGID spatialMap =
  case Data.Map.Strict.lookup objGID spatialMap of
    Just relationships
      | Inventory `Data.Set.member` relationships ->
          modifyNarration $ updateActionConsequence $ "You're holding the " <> _description obj
      | Just containerGID <- findContainer relationships -> do
          modifyNarration $ updateActionConsequence $  _description obj
      | otherwise ->
          modifyNarration $ updateActionConsequence $ "You see the " <> _shortName obj
    Nothing ->
      modifyNarration $ updateActionConsequence $ "You see the " <> _shortName obj

generateContentsNarration :: GID Object
                          -> Data.Map.Strict.Map (GID Object) (Data.Set.Set SpatialRelationship)
                          -> GameComputation Identity ()
generateContentsNarration objGID spatialMap = do
  case Data.Map.Strict.lookup objGID spatialMap of
    Nothing -> error $ "Programmer Error: Object " <> show objGID <> " not found in SpatialRelationshipMap"
    Just objRelationships -> do
      -- Use objRelationships directly instead of calling helper functions
      let supportedObjects = [oid | Supports oidSet <- Data.Set.toList objRelationships,
                                    oid <- Data.Set.toList oidSet]
      let containedObjects = [oid | Contains oidSet <- Data.Set.toList objRelationships,
                                    oid <- Data.Set.toList oidSet]

      -- Filter to only perceivable objects
      perceivableSupportedObjects <- filterM isObjectPerceivable supportedObjects
      perceivableContainedObjects <- filterM isObjectPerceivable containedObjects

      -- Generate narration for supported objects (on the object)
      unless (null perceivableSupportedObjects) $ do
        supportedNames <- mapM (fmap _shortName . getObjectM) perceivableSupportedObjects
        let onText = "On it you see: " <> Data.Text.intercalate ", " supportedNames
        modifyNarration $ updateActionConsequence onText

      -- Generate narration for contained objects (in the object)
      unless (null perceivableContainedObjects) $ do
        containedNames <- mapM (fmap _shortName . getObjectM) perceivableContainedObjects
        let inText = "In it you see: " <> Data.Text.intercalate ", " containedNames
        modifyNarration $ updateActionConsequence inText
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
    actionEnabled :: ActionEffectKey -> ImplicitStimulusActionMap -> GameComputation Identity ()
    actionEnabled pActionKey pActionMap = do
      loc <- getPlayerLocationM
      let actionMgmt = _locationActionManagement loc
      case lookupImplicitStimulus isv actionMgmt of
        Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in location map"
        Just actionGID -> do
          actionMap' :: Map (GID ImplicitStimulusActionF) ImplicitStimulusActionF <- asks (_implicitStimulusActionMap . _actionMaps)
          case Data.Map.Strict.lookup actionGID actionMap' of
            Nothing -> error "Programmer Error: No implicit stimulus action found for verb: in actionmap "
            Just isvAction -> do
              case isvAction of
                (PlayerImplicitStimulusActionF actionFunc) ->
                  let actionKey = ImplicitStimulusActionKey actionGID
                  in actionFunc actionKey actionMap'
                (CannotImplicitStimulusActionF actionFunc) ->
                  let actionKey = ImplicitStimulusActionKey actionGID
                  in actionFunc actionKey actionMap'

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
