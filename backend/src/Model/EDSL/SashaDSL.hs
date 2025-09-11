module Model.EDSL.SashaDSL where

import           Control.Monad                 (foldM)
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Model.Core                    (AcquisitionActionF,
                                                ActionEffectKey,
                                                ActionManagement,
                                                ConsumptionActionF,
                                                ContainerAccessActionF,
                                                DirectionalStimulusActionF,
                                                DirectionalStimulusContainerActionF,
                                                Effect, Evaluator,
                                                GameComputation, GameState,
                                                ImplicitStimulusActionF,
                                                Location, Object, Player,
                                                PlayerKey, PosturalActionF,
                                                SomaticAccessActionF,
                                                SpatialRelationship,
                                                SystemEffect,
                                                SystemEffectConfig,
                                                SystemEffectKey)
import           Model.GID                     (GID)
import           Model.Parser.Atomics.Nouns    (Consumable, Container,
                                                DirectionalStimulus, Objective)
import           Model.Parser.Atomics.Verbs    (AcquisitionVerb,
                                                ConsumptionVerb,
                                                DirectionalStimulusVerb,
                                                ImplicitStimulusVerb,
                                                NegativePosturalVerb,
                                                PositivePosturalVerb,
                                                SimpleAccessVerb,
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase,
                                                NounPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase)
import           Model.Parser.GCase            (NounKey)


-- | SashaDSL is the intermediate language for building game worlds.
type SashaDSL :: Type -> Type
data SashaDSL :: Type -> Type where
  -- Pure values
  Pure :: a -> SashaDSL a

  -- Functor operations
  Map :: (a -> b) -> SashaDSL a -> SashaDSL b

  -- Applicative operations
  Apply :: SashaDSL (a -> b) -> SashaDSL a -> SashaDSL b
  Sequence :: SashaDSL a -> SashaDSL b -> SashaDSL b

  -- Monadic operations
  Bind :: SashaDSL a -> (a -> SashaDSL b) -> SashaDSL b

  -- GID Declaration constructors
  DeclareObjectGID :: NounPhrase DirectionalStimulus -> SashaDSL (GID Object)
  DeclareObjectiveGID :: GID Object -> NounPhrase Objective -> SashaDSL ()
  DeclareConsumableGID :: GID Object -> NounPhrase Consumable -> SashaDSL ()
  DeclareContainerGID :: GID Object -> NounPhrase Container -> SashaDSL ()
  DeclareLocationGID :: NounPhrase DirectionalStimulus -> SashaDSL (GID Location)

  DeclareImplicitStimulusActionGID :: ImplicitStimulusActionF -> SashaDSL (GID ImplicitStimulusActionF)
  DeclareDirectionalStimulusActionGID :: DirectionalStimulusActionF -> SashaDSL (GID DirectionalStimulusActionF)
  DeclareDirectionalContainerActionGID :: DirectionalStimulusContainerActionF -> SashaDSL (GID DirectionalStimulusContainerActionF)
  DeclareSomaticActionGID :: SomaticAccessActionF -> SashaDSL (GID SomaticAccessActionF)
  DeclareAcquisitionActionGID :: AcquisitionActionF -> SashaDSL (GID AcquisitionActionF)
  DeclareConsumptionActionGID :: ConsumptionActionF -> SashaDSL (GID ConsumptionActionF)
  DeclarePosturalActionGID :: PosturalActionF -> SashaDSL (GID PosturalActionF)
  DeclareContainerAccessActionGID :: ContainerAccessActionF -> SashaDSL (GID ContainerAccessActionF)
  WithShortName :: Text -> Object -> SashaDSL Object
  WithDescription :: Text -> Object -> SashaDSL Object
  WithDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> SashaDSL Object
  WithTitle :: Text -> Location -> SashaDSL Location

  CreateISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaDSL ActionManagement
  CreateDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaDSL ActionManagement
  CreateDSAContainerManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaDSL ActionManagement
  CreateSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaDSL ActionManagement
  CreateAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> SashaDSL ActionManagement
  CreateAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaDSL ActionManagement
  CreateCAManagement :: ConsumptionVerb -> GID ConsumptionActionF -> SashaDSL ActionManagement
  CreateSAConManagement :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaDSL ActionManagement
  CreatePPManagement :: PositivePosturalVerb -> GID PosturalActionF -> SashaDSL ActionManagement
  CreateNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> SashaDSL ActionManagement

  SetPerceptionMap :: [(DirectionalStimulusNounPhrase, [GID Object])] -> SashaDSL ()
  SetEvaluator :: Evaluator -> SashaDSL ()
  SetInitialNarration :: Text -> SashaDSL ()

  WithPlayerLocation :: Player -> GID Location -> SashaDSL Player -- NEW: Clean player location setting

  WithObjectBehavior :: Object -> ActionManagement -> SashaDSL Object
  WithLocationBehavior :: Location -> ActionManagement -> SashaDSL Location
  WithPlayerBehavior :: Player -> ActionManagement -> SashaDSL Player

  UpdateShortName :: Text -> GID Object -> SashaDSL Effect
  UpdateDescription :: Text -> GID Object -> SashaDSL Effect
  UpdateTitle :: Text -> GID Location -> SashaDSL Effect
  UpdateLocation :: GID Location -> SashaDSL Effect

-- Map registration constructors
  RegisterObject :: GID Object -> SashaDSL Object -> SashaDSL ()
  RegisterLocation :: GID Location -> SashaDSL Location -> SashaDSL ()
  RegisterPlayer :: Player -> SashaDSL ()
  RegisterSpatial :: GID Object -> SpatialRelationship -> SashaDSL ()
  RegisterObjectToLocation :: GID Location -> GID Object -> NounKey -> SashaDSL ()
  RegisterSystemEffect :: SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> SashaDSL ()
  RegisterTrigger :: ActionEffectKey -> SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> SashaDSL ()
  -- Effect management
  CreateImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaDSL Effect
  CreateDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaDSL Effect
  CreateDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaDSL Effect
  CreateAcquisitionVerbEffect :: AcquisitionVerb -> GID AcquisitionActionF -> SashaDSL Effect
  CreateAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaDSL Effect
  CreateConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> SashaDSL Effect
  CreatePositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> SashaDSL Effect
  CreateNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> SashaDSL Effect
  CreateSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaDSL Effect
  CreateContainerAccessEffect :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaDSL Effect

  -- Narration effect creation
  CreatePlayerActionNarration :: Text -> SashaDSL Effect
  CreateActionConsequenceNarration :: Text -> SashaDSL Effect

  LinkFieldEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaDSL ()
  LinkFieldEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaDSL ()
  LinkFieldEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaDSL ()

  LinkEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaDSL ()
  LinkEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaDSL ()
  LinkEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaDSL ()
  LinkActionKeyToSystemEffect :: ActionEffectKey -> SystemEffectKey -> SashaDSL ()
  -- Final assembly
  FinalizeGameState :: SashaDSL GameState

instance Functor SashaDSL where
  fmap = Map

instance Applicative SashaDSL where
  pure = Pure
  (<*>) = Apply

instance Monad SashaDSL where
   return = pure
   (>>=) = Bind

declareObjectGID :: NounPhrase DirectionalStimulus -> SashaDSL (GID Object)
declareObjectGID  = DeclareObjectGID

declareObjectiveGID :: GID Object -> NounPhrase Objective -> SashaDSL ()
declareObjectiveGID = DeclareObjectiveGID

declareConsumableGID :: GID Object -> NounPhrase Consumable -> SashaDSL ()
declareConsumableGID = DeclareConsumableGID

declareContainerGID :: GID Object -> NounPhrase Container -> SashaDSL ()
declareContainerGID = DeclareContainerGID

declareLocationGID :: NounPhrase DirectionalStimulus -> SashaDSL (GID Location)
declareLocationGID = DeclareLocationGID

declareImplicitStimulusActionGID :: ImplicitStimulusActionF -> SashaDSL (GID ImplicitStimulusActionF)
declareImplicitStimulusActionGID = DeclareImplicitStimulusActionGID

declareDirectionalStimulusActionGID :: DirectionalStimulusActionF -> SashaDSL (GID DirectionalStimulusActionF)
declareDirectionalStimulusActionGID = DeclareDirectionalStimulusActionGID

declareDirectionalContainerActionGID :: DirectionalStimulusContainerActionF -> SashaDSL (GID DirectionalStimulusContainerActionF)
declareDirectionalContainerActionGID = DeclareDirectionalContainerActionGID

declareSomaticActionGID :: SomaticAccessActionF -> SashaDSL (GID SomaticAccessActionF)
declareSomaticActionGID = DeclareSomaticActionGID

declareAcquisitionActionGID :: AcquisitionActionF -> SashaDSL (GID AcquisitionActionF)
declareAcquisitionActionGID = DeclareAcquisitionActionGID

declareConsumptionActionGID :: ConsumptionActionF -> SashaDSL (GID ConsumptionActionF)
declareConsumptionActionGID = DeclareConsumptionActionGID

declarePosturalActionGID :: PosturalActionF -> SashaDSL (GID PosturalActionF)
declarePosturalActionGID = DeclarePosturalActionGID

declareContainerAccessActionGID :: ContainerAccessActionF -> SashaDSL (GID ContainerAccessActionF)
declareContainerAccessActionGID = DeclareContainerAccessActionGID

createISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaDSL ActionManagement
createISAManagement = CreateISAManagement

createDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaDSL ActionManagement
createDSAManagement = CreateDSAManagement

createDSAContainerManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaDSL ActionManagement
createDSAContainerManagement = CreateDSAContainerManagement

createSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaDSL ActionManagement
createSSAManagement = CreateSSAManagement

createSAConManagement :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaDSL ActionManagement
createSAConManagement = CreateSAConManagement

createAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> SashaDSL ActionManagement
createAVManagement = CreateAVManagement

createCAManagement :: ConsumptionVerb -> GID ConsumptionActionF -> SashaDSL ActionManagement
createCAManagement = CreateCAManagement

createPPManagement :: PositivePosturalVerb -> GID PosturalActionF -> SashaDSL ActionManagement
createPPManagement = CreatePPManagement

createNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> SashaDSL ActionManagement
createNPManagement = CreateNPManagement

withPlayerLocation :: Player -> GID Location ->  SashaDSL Player
withPlayerLocation = WithPlayerLocation

withObjectBehavior :: Object -> ActionManagement -> SashaDSL Object
withObjectBehavior = WithObjectBehavior

withLocationBehavior :: Location -> ActionManagement -> SashaDSL Location
withLocationBehavior = WithLocationBehavior

withPlayerBehavior :: Player -> ActionManagement -> SashaDSL Player
withPlayerBehavior = WithPlayerBehavior

withPlayerBehaviors :: Player -> [ActionManagement] -> SashaDSL Player
withPlayerBehaviors = foldM withPlayerBehavior

createImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaDSL Effect
createImplicitStimulusEffect = CreateImplicitStimulusEffect

createDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaDSL Effect
createDirectionalStimulusEffect = CreateDirectionalStimulusEffect

createDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaDSL Effect
createDirectionalContainerStimulusEffect = CreateDirectionalContainerStimulusEffect

createAcquisitionVerbEffect :: AcquisitionVerb -> GID AcquisitionActionF -> SashaDSL Effect
createAcquisitionVerbEffect = CreateAcquisitionVerbEffect

createAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaDSL Effect
createAcquisitionVerbPhraseEffect = CreateAcquisitionVerbPhraseEffect

createAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaDSL ActionManagement
createAAManagement = CreateAAManagement

createConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> SashaDSL Effect
createConsumptionEffect = CreateConsumptionEffect

createPositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> SashaDSL Effect
createPositivePosturalEffect = CreatePositivePosturalEffect

createNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> SashaDSL Effect
createNegativePosturalEffect = CreateNegativePosturalEffect

createSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaDSL Effect
createSomaticAccessEffect = CreateSomaticAccessEffect

createContainerAccessEffect :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaDSL Effect
createContainerAccessEffect = CreateContainerAccessEffect

createPlayerActionNarration :: Text -> SashaDSL Effect
createPlayerActionNarration = CreatePlayerActionNarration

createActionConsequenceNarration :: Text -> SashaDSL Effect
createActionConsequenceNarration = CreateActionConsequenceNarration

linkEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaDSL ()
linkEffectToObject = LinkEffectToObject

linkEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaDSL ()
linkEffectToLocation = LinkEffectToLocation

linkEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaDSL ()
linkEffectToPlayer = LinkEffectToPlayer

linkActionKeyToSystemEffect :: ActionEffectKey -> SystemEffectKey -> SashaDSL ()
linkActionKeyToSystemEffect = LinkActionKeyToSystemEffect

finalizeGameState :: SashaDSL GameState
finalizeGameState = FinalizeGameState

-- Convenience functions
buildSequentially :: SashaDSL a -> SashaDSL b -> SashaDSL b
buildSequentially = Sequence

setEvaluator :: Evaluator -> SashaDSL ()
setEvaluator = SetEvaluator

setInitialNarration :: Text -> SashaDSL ()
setInitialNarration = SetInitialNarration

setPerceptionMap :: [(DirectionalStimulusNounPhrase, [GID Object])] -> SashaDSL ()
setPerceptionMap = SetPerceptionMap

-- Object field setters
withShortName :: Text -> Object -> SashaDSL Object
withShortName = WithShortName

withDescription :: Text -> Object -> SashaDSL Object
withDescription = WithDescription

withDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> SashaDSL Object
withDescriptives = WithDescriptives

-- Location field setters
withTitle :: Text -> Location -> SashaDSL Location
withTitle = WithTitle

-- Replace existing functions:
registerObject :: GID Object -> SashaDSL Object -> SashaDSL ()
registerObject = RegisterObject

registerLocation :: GID Location -> SashaDSL Location -> SashaDSL ()
registerLocation = RegisterLocation

registerPlayer :: Player -> SashaDSL ()
registerPlayer = RegisterPlayer

registerSpatial :: GID Object -> SpatialRelationship -> SashaDSL ()
registerSpatial = RegisterSpatial

registerObjectToLocation :: GID Location -> GID Object -> NounKey -> SashaDSL ()
registerObjectToLocation = RegisterObjectToLocation

registerSystemEffect :: SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> SashaDSL ()
registerSystemEffect = RegisterSystemEffect

registerTrigger :: ActionEffectKey
                     -> SystemEffectKey
                     -> GID SystemEffect
                     -> SystemEffectConfig
                     -> SashaDSL ()
registerTrigger = RegisterTrigger

updateShortName :: Text -> GID Object -> SashaDSL Effect
updateShortName = UpdateShortName

updateDescription :: Text -> GID Object -> SashaDSL Effect
updateDescription = UpdateDescription

updateTitle :: Text -> GID Location -> SashaDSL Effect
updateTitle = UpdateTitle

updateLocation :: GID Location -> SashaDSL Effect
updateLocation = UpdateLocation

linkFieldEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaDSL ()
linkFieldEffectToObject = LinkFieldEffectToObject

linkFieldEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaDSL ()
linkFieldEffectToLocation = LinkFieldEffectToLocation

linkFieldEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaDSL ()
linkFieldEffectToPlayer = LinkFieldEffectToPlayer
