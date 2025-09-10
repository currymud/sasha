module Model.EDSL.SashaLambdaDSL where

import           Control.Monad                 (foldM)
import           Control.Monad.Identity        (Identity)
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Model.Core                    (AcquisitionActionF,
                                                ActionManagement,
                                                ConsumptionActionF,
                                                ContainerAccessActionF,
                                                DirectionalStimulusActionF,
                                                DirectionalStimulusContainerActionF,
                                                Effect, EffectActionKey,
                                                Evaluator, GameComputation,
                                                GameState,
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


-- | SashaLambdaDSL is the intermediate language for building game worlds.
type SashaLambdaDSL :: Type -> Type
data SashaLambdaDSL :: Type -> Type where
  -- Pure values
  Pure :: a -> SashaLambdaDSL a

  -- Functor operations
  Map :: (a -> b) -> SashaLambdaDSL a -> SashaLambdaDSL b

  -- Applicative operations
  Apply :: SashaLambdaDSL (a -> b) -> SashaLambdaDSL a -> SashaLambdaDSL b
  Sequence :: SashaLambdaDSL a -> SashaLambdaDSL b -> SashaLambdaDSL b

  -- Monadic operations
  Bind :: SashaLambdaDSL a -> (a -> SashaLambdaDSL b) -> SashaLambdaDSL b

  -- GID Declaration constructors
  DeclareObjectGID :: NounPhrase DirectionalStimulus -> SashaLambdaDSL (GID Object)
  DeclareObjectiveGID :: GID Object -> NounPhrase Objective -> SashaLambdaDSL ()
  DeclareConsumableGID :: GID Object -> NounPhrase Consumable -> SashaLambdaDSL ()
  DeclareContainerGID :: GID Object -> NounPhrase Container -> SashaLambdaDSL ()
  DeclareLocationGID :: NounPhrase DirectionalStimulus -> SashaLambdaDSL (GID Location)

  DeclareImplicitStimulusActionGID :: ImplicitStimulusActionF -> SashaLambdaDSL (GID ImplicitStimulusActionF)
  DeclareDirectionalStimulusActionGID :: DirectionalStimulusActionF -> SashaLambdaDSL (GID DirectionalStimulusActionF)
  DeclareDirectionalContainerActionGID :: DirectionalStimulusContainerActionF -> SashaLambdaDSL (GID DirectionalStimulusContainerActionF)
  DeclareSomaticActionGID :: SomaticAccessActionF -> SashaLambdaDSL (GID SomaticAccessActionF)
  DeclareAcquisitionActionGID :: AcquisitionActionF -> SashaLambdaDSL (GID AcquisitionActionF)
  DeclareConsumptionActionGID :: ConsumptionActionF -> SashaLambdaDSL (GID ConsumptionActionF)
  DeclarePosturalActionGID :: PosturalActionF -> SashaLambdaDSL (GID PosturalActionF)
  DeclareContainerAccessActionGID :: ContainerAccessActionF -> SashaLambdaDSL (GID ContainerAccessActionF)
  WithShortName :: Text -> Object -> SashaLambdaDSL Object
  WithDescription :: Text -> Object -> SashaLambdaDSL Object
  WithDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> SashaLambdaDSL Object
  WithTitle :: Text -> Location -> SashaLambdaDSL Location

  CreateISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaLambdaDSL ActionManagement
  CreateDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaLambdaDSL ActionManagement
  CreateDSAContainerManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
  CreateSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaLambdaDSL ActionManagement
  CreateAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> SashaLambdaDSL ActionManagement
  CreateAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaLambdaDSL ActionManagement
  CreateCAManagement :: ConsumptionVerb -> GID ConsumptionActionF -> SashaLambdaDSL ActionManagement
  CreateSAConManagement :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaLambdaDSL ActionManagement
  CreatePPManagement :: PositivePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL ActionManagement
  CreateNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL ActionManagement

  SetPerceptionMap :: [(DirectionalStimulusNounPhrase, [GID Object])] -> SashaLambdaDSL ()
  SetEvaluator :: Evaluator -> SashaLambdaDSL ()
  SetInitialNarration :: Text -> SashaLambdaDSL ()

  WithPlayerLocation :: Player -> GID Location -> SashaLambdaDSL Player -- NEW: Clean player location setting

  WithObjectBehavior :: Object -> ActionManagement -> SashaLambdaDSL Object
  WithLocationBehavior :: Location -> ActionManagement -> SashaLambdaDSL Location
  WithPlayerBehavior :: Player -> ActionManagement -> SashaLambdaDSL Player

  UpdateShortName :: Text -> GID Object -> SashaLambdaDSL Effect
  UpdateDescription :: Text -> GID Object -> SashaLambdaDSL Effect
  UpdateTitle :: Text -> GID Location -> SashaLambdaDSL Effect
  UpdateLocation :: GID Location -> SashaLambdaDSL Effect

-- Map registration constructors
  RegisterObject :: GID Object -> SashaLambdaDSL Object -> SashaLambdaDSL ()
  RegisterLocation :: GID Location -> SashaLambdaDSL Location -> SashaLambdaDSL ()
  RegisterPlayer :: Player -> SashaLambdaDSL ()
  RegisterSpatial :: GID Object -> SpatialRelationship -> SashaLambdaDSL ()
  RegisterObjectToLocation :: GID Location -> GID Object -> NounKey -> SashaLambdaDSL ()
  RegisterSystemEffect :: SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> SashaLambdaDSL ()
  RegisterTrigger :: EffectActionKey -> SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> SashaLambdaDSL ()
  -- Effect management
  CreateImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaLambdaDSL Effect
  CreateDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaLambdaDSL Effect
  CreateDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
  CreateAcquisitionVerbEffect :: AcquisitionVerb -> GID AcquisitionActionF -> SashaLambdaDSL Effect
  CreateAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaLambdaDSL Effect
  CreateConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> SashaLambdaDSL Effect
  CreatePositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL Effect
  CreateNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL Effect
  CreateSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaLambdaDSL Effect
  CreateContainerAccessEffect :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaLambdaDSL Effect

  LinkFieldEffectToObject :: EffectActionKey -> GID Object -> Effect -> SashaLambdaDSL ()
  LinkFieldEffectToLocation :: EffectActionKey -> GID Location -> Effect -> SashaLambdaDSL ()
  LinkFieldEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> SashaLambdaDSL ()

  LinkEffectToObject :: EffectActionKey -> GID Object -> Effect -> SashaLambdaDSL ()
  LinkEffectToLocation :: EffectActionKey -> GID Location -> Effect -> SashaLambdaDSL ()
  LinkEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> SashaLambdaDSL ()
  LinkActionKeyToSystemEffect :: EffectActionKey -> SystemEffectKey -> SashaLambdaDSL ()
  DisplayVisibleObjects :: SashaLambdaDSL (GameComputation Identity ())
  -- Final assembly
  FinalizeGameState :: SashaLambdaDSL GameState

instance Functor SashaLambdaDSL where
  fmap = Map

instance Applicative SashaLambdaDSL where
  pure = Pure
  (<*>) = Apply

instance Monad SashaLambdaDSL where
   return = pure
   (>>=) = Bind

declareObjectGID :: NounPhrase DirectionalStimulus -> SashaLambdaDSL (GID Object)
declareObjectGID  = DeclareObjectGID

declareObjectiveGID :: GID Object -> NounPhrase Objective -> SashaLambdaDSL ()
declareObjectiveGID = DeclareObjectiveGID

declareConsumableGID :: GID Object -> NounPhrase Consumable -> SashaLambdaDSL ()
declareConsumableGID = DeclareConsumableGID

declareContainerGID :: GID Object -> NounPhrase Container -> SashaLambdaDSL ()
declareContainerGID = DeclareContainerGID

declareLocationGID :: NounPhrase DirectionalStimulus -> SashaLambdaDSL (GID Location)
declareLocationGID = DeclareLocationGID

declareImplicitStimulusActionGID :: ImplicitStimulusActionF -> SashaLambdaDSL (GID ImplicitStimulusActionF)
declareImplicitStimulusActionGID = DeclareImplicitStimulusActionGID

declareDirectionalStimulusActionGID :: DirectionalStimulusActionF -> SashaLambdaDSL (GID DirectionalStimulusActionF)
declareDirectionalStimulusActionGID = DeclareDirectionalStimulusActionGID

declareDirectionalContainerActionGID :: DirectionalStimulusContainerActionF -> SashaLambdaDSL (GID DirectionalStimulusContainerActionF)
declareDirectionalContainerActionGID = DeclareDirectionalContainerActionGID

declareSomaticActionGID :: SomaticAccessActionF -> SashaLambdaDSL (GID SomaticAccessActionF)
declareSomaticActionGID = DeclareSomaticActionGID

declareAcquisitionActionGID :: AcquisitionActionF -> SashaLambdaDSL (GID AcquisitionActionF)
declareAcquisitionActionGID = DeclareAcquisitionActionGID

declareConsumptionActionGID :: ConsumptionActionF -> SashaLambdaDSL (GID ConsumptionActionF)
declareConsumptionActionGID = DeclareConsumptionActionGID

declarePosturalActionGID :: PosturalActionF -> SashaLambdaDSL (GID PosturalActionF)
declarePosturalActionGID = DeclarePosturalActionGID

declareContainerAccessActionGID :: ContainerAccessActionF -> SashaLambdaDSL (GID ContainerAccessActionF)
declareContainerAccessActionGID = DeclareContainerAccessActionGID

createISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaLambdaDSL ActionManagement
createISAManagement = CreateISAManagement

createDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaLambdaDSL ActionManagement
createDSAManagement = CreateDSAManagement

createDSAContainerManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
createDSAContainerManagement = CreateDSAContainerManagement

createSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaLambdaDSL ActionManagement
createSSAManagement = CreateSSAManagement

createSAConManagement :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaLambdaDSL ActionManagement
createSAConManagement = CreateSAConManagement

createAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> SashaLambdaDSL ActionManagement
createAVManagement = CreateAVManagement

createCAManagement :: ConsumptionVerb -> GID ConsumptionActionF -> SashaLambdaDSL ActionManagement
createCAManagement = CreateCAManagement

createPPManagement :: PositivePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL ActionManagement
createPPManagement = CreatePPManagement

createNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL ActionManagement
createNPManagement = CreateNPManagement

withPlayerLocation :: Player -> GID Location ->  SashaLambdaDSL Player
withPlayerLocation = WithPlayerLocation

withObjectBehavior :: Object -> ActionManagement -> SashaLambdaDSL Object
withObjectBehavior = WithObjectBehavior

withLocationBehavior :: Location -> ActionManagement -> SashaLambdaDSL Location
withLocationBehavior = WithLocationBehavior

withPlayerBehavior :: Player -> ActionManagement -> SashaLambdaDSL Player
withPlayerBehavior = WithPlayerBehavior

withPlayerBehaviors :: Player -> [ActionManagement] -> SashaLambdaDSL Player
withPlayerBehaviors = foldM withPlayerBehavior

createImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> SashaLambdaDSL Effect
createImplicitStimulusEffect = CreateImplicitStimulusEffect

createDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> SashaLambdaDSL Effect
createDirectionalStimulusEffect = CreateDirectionalStimulusEffect

createDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
createDirectionalContainerStimulusEffect = CreateDirectionalContainerStimulusEffect

createAcquisitionVerbEffect :: AcquisitionVerb -> GID AcquisitionActionF -> SashaLambdaDSL Effect
createAcquisitionVerbEffect = CreateAcquisitionVerbEffect

createAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaLambdaDSL Effect
createAcquisitionVerbPhraseEffect = CreateAcquisitionVerbPhraseEffect

createAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> SashaLambdaDSL ActionManagement
createAAManagement = CreateAAManagement

createConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> SashaLambdaDSL Effect
createConsumptionEffect = CreateConsumptionEffect

createPositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL Effect
createPositivePosturalEffect = CreatePositivePosturalEffect

createNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL Effect
createNegativePosturalEffect = CreateNegativePosturalEffect

createSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaLambdaDSL Effect
createSomaticAccessEffect = CreateSomaticAccessEffect

createContainerAccessEffect :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaLambdaDSL Effect
createContainerAccessEffect = CreateContainerAccessEffect

linkEffectToObject :: EffectActionKey -> GID Object -> Effect -> SashaLambdaDSL ()
linkEffectToObject = LinkEffectToObject

linkEffectToLocation :: EffectActionKey -> GID Location -> Effect -> SashaLambdaDSL ()
linkEffectToLocation = LinkEffectToLocation

linkEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> SashaLambdaDSL ()
linkEffectToPlayer = LinkEffectToPlayer

linkActionKeyToSystemEffect :: EffectActionKey -> SystemEffectKey -> SashaLambdaDSL ()
linkActionKeyToSystemEffect = LinkActionKeyToSystemEffect

finalizeGameState :: SashaLambdaDSL GameState
finalizeGameState = FinalizeGameState

-- Convenience functions
buildSequentially :: SashaLambdaDSL a -> SashaLambdaDSL b -> SashaLambdaDSL b
buildSequentially = Sequence

setEvaluator :: Evaluator -> SashaLambdaDSL ()
setEvaluator = SetEvaluator

setInitialNarration :: Text -> SashaLambdaDSL ()
setInitialNarration = SetInitialNarration

setPerceptionMap :: [(DirectionalStimulusNounPhrase, [GID Object])] -> SashaLambdaDSL ()
setPerceptionMap = SetPerceptionMap

-- Object field setters
withShortName :: Text -> Object -> SashaLambdaDSL Object
withShortName = WithShortName

withDescription :: Text -> Object -> SashaLambdaDSL Object
withDescription = WithDescription

withDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> SashaLambdaDSL Object
withDescriptives = WithDescriptives

-- Location field setters
withTitle :: Text -> Location -> SashaLambdaDSL Location
withTitle = WithTitle

-- Replace existing functions:
registerObject :: GID Object -> SashaLambdaDSL Object -> SashaLambdaDSL ()
registerObject = RegisterObject

registerLocation :: GID Location -> SashaLambdaDSL Location -> SashaLambdaDSL ()
registerLocation = RegisterLocation

registerPlayer :: Player -> SashaLambdaDSL ()
registerPlayer = RegisterPlayer

registerSpatial :: GID Object -> SpatialRelationship -> SashaLambdaDSL ()
registerSpatial = RegisterSpatial

registerObjectToLocation :: GID Location -> GID Object -> NounKey -> SashaLambdaDSL ()
registerObjectToLocation = RegisterObjectToLocation

registerSystemEffect :: SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> SashaLambdaDSL ()
registerSystemEffect = RegisterSystemEffect

registerTrigger :: EffectActionKey
                     -> SystemEffectKey
                     -> GID SystemEffect
                     -> SystemEffectConfig
                     -> SashaLambdaDSL ()
registerTrigger = RegisterTrigger

displayVisibleObjects :: SashaLambdaDSL (GameComputation Identity ())
displayVisibleObjects = DisplayVisibleObjects

updateShortName :: Text -> GID Object -> SashaLambdaDSL Effect
updateShortName = UpdateShortName

updateDescription :: Text -> GID Object -> SashaLambdaDSL Effect
updateDescription = UpdateDescription

updateTitle :: Text -> GID Location -> SashaLambdaDSL Effect
updateTitle = UpdateTitle

updateLocation :: GID Location -> SashaLambdaDSL Effect
updateLocation = UpdateLocation

linkFieldEffectToObject :: EffectActionKey -> GID Object -> Effect -> SashaLambdaDSL ()
linkFieldEffectToObject = LinkFieldEffectToObject

linkFieldEffectToLocation :: EffectActionKey -> GID Location -> Effect -> SashaLambdaDSL ()
linkFieldEffectToLocation = LinkFieldEffectToLocation

linkFieldEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> SashaLambdaDSL ()
linkFieldEffectToPlayer = LinkFieldEffectToPlayer
