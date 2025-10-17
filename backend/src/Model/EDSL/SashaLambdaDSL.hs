module Model.EDSL.SashaLambdaDSL where

import           Control.Monad                 (foldM)
import           Control.Monad.Identity        (Identity)
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Model.Core                    (ActionEffectKey,
                                                ActionManagement,
                                                AgentAcquisitionActionF,
                                                AgentDirectionalStimulusActionF,
                                                AgentDirectionalStimulusContainerActionF,
                                                AgentImplicitStimulusActionF,
                                                ConsumptionActionF,
                                                ContainerAccessActionF,
                                                ContainerAcquisitionActionF,
                                                ContainerDirectionalStimulusContainerActionF,
                                                DirectionalStimulusContainerActionF,
                                                Effect, Evaluator,
                                                GameComputation, GameState,
                                                Location,
                                                LocationAcquisitionActionF,
                                                LocationDirectionalStimulusActionF,
                                                LocationDirectionalStimulusContainerActionF,
                                                LocationImplicitStimulusActionF,
                                                Object,
                                                ObjectAcquisitionActionF,
                                                ObjectDirectionalStimulusActionF,
                                                Player, PlayerKey,
                                                PosturalActionF,
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
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ContainerAccessVerbPhrase)
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

  -- Role-based implicit stimulus action declarations
  DeclareAgentImplicitStimulusActionGID :: AgentImplicitStimulusActionF -> SashaLambdaDSL (GID AgentImplicitStimulusActionF)
  DeclareLocationImplicitStimulusActionGID :: LocationImplicitStimulusActionF -> SashaLambdaDSL (GID LocationImplicitStimulusActionF)
  -- Role-based directional stimulus action declarations
  DeclareAgentDirectionalStimulusActionGID :: AgentDirectionalStimulusActionF -> SashaLambdaDSL (GID AgentDirectionalStimulusActionF)
  DeclareObjectDirectionalStimulusActionGID :: ObjectDirectionalStimulusActionF -> SashaLambdaDSL (GID ObjectDirectionalStimulusActionF)
  DeclareLocationDirectionalStimulusActionGID :: LocationDirectionalStimulusActionF -> SashaLambdaDSL (GID LocationDirectionalStimulusActionF)
  DeclareDirectionalContainerActionGID :: DirectionalStimulusContainerActionF -> SashaLambdaDSL (GID DirectionalStimulusContainerActionF)
  -- Role-based directional container stimulus action declarations
  DeclareAgentDirectionalContainerStimulusActionGID :: AgentDirectionalStimulusContainerActionF -> SashaLambdaDSL (GID AgentDirectionalStimulusContainerActionF)
  DeclareContainerDirectionalContainerStimulusActionGID :: ContainerDirectionalStimulusContainerActionF -> SashaLambdaDSL (GID ContainerDirectionalStimulusContainerActionF)
  DeclareLocationDirectionalContainerStimulusActionGID :: LocationDirectionalStimulusContainerActionF -> SashaLambdaDSL (GID LocationDirectionalStimulusContainerActionF)
  DeclareSomaticActionGID :: SomaticAccessActionF -> SashaLambdaDSL (GID SomaticAccessActionF)
  -- Role-based acquisition action declarations
  DeclareAgentAcquisitionActionGID :: AgentAcquisitionActionF -> SashaLambdaDSL (GID AgentAcquisitionActionF)
  DeclareObjectAcquisitionActionGID :: ObjectAcquisitionActionF -> SashaLambdaDSL (GID ObjectAcquisitionActionF)
  DeclareContainerAcquisitionActionGID :: ContainerAcquisitionActionF -> SashaLambdaDSL (GID ContainerAcquisitionActionF)
  DeclareLocationAcquisitionActionGID :: LocationAcquisitionActionF -> SashaLambdaDSL (GID LocationAcquisitionActionF)
  DeclareConsumptionActionGID :: ConsumptionActionF -> SashaLambdaDSL (GID ConsumptionActionF)
  DeclarePosturalActionGID :: PosturalActionF -> SashaLambdaDSL (GID PosturalActionF)
  DeclareContainerAccessActionGID :: ContainerAccessActionF -> SashaLambdaDSL (GID ContainerAccessActionF)
  WithShortName :: Text -> Object -> SashaLambdaDSL Object
  WithDescription :: Text -> Object -> SashaLambdaDSL Object
  WithDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> SashaLambdaDSL Object
  WithTitle :: Text -> Location -> SashaLambdaDSL Location

  -- Role-based implicit stimulus management creation
  CreateAgentISAManagement :: ImplicitStimulusVerb -> GID AgentImplicitStimulusActionF -> SashaLambdaDSL ActionManagement
  CreateLocationISAManagement :: ImplicitStimulusVerb -> GID LocationImplicitStimulusActionF -> SashaLambdaDSL ActionManagement
  CreateDSAContainerManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
  -- Role-based directional container stimulus management creation
  CreateAgentDSAContainerManagement :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
  CreateContainerDSAContainerManagement :: DirectionalStimulusVerb -> GID ContainerDirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
  CreateLocationDSAContainerManagement :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
  CreateSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaLambdaDSL ActionManagement
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
  RegisterTrigger :: ActionEffectKey -> SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> SashaLambdaDSL ()
  -- Effect management
  -- Role-based implicit stimulus effect creation constructors
  CreateAgentImplicitStimulusEffect :: ImplicitStimulusVerb -> GID AgentImplicitStimulusActionF -> SashaLambdaDSL Effect
  CreateLocationImplicitStimulusEffect :: ImplicitStimulusVerb -> GID LocationImplicitStimulusActionF -> SashaLambdaDSL Effect
  CreateDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
  -- Role-based directional stimulus effect creation constructors
  CreateAgentDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusActionF -> SashaLambdaDSL Effect
  CreateObjectDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID ObjectDirectionalStimulusActionF -> SashaLambdaDSL Effect
  CreateLocationDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusActionF -> SashaLambdaDSL Effect
  -- Role-based directional container stimulus effect creation constructors
  CreateAgentDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
  CreateContainerDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID ContainerDirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
  CreateLocationDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
  -- Role-based acquisition effect creation constructors
  CreateAgentAcquisitionVerbEffect :: AcquisitionVerb -> GID AgentAcquisitionActionF -> SashaLambdaDSL Effect
  CreateObjectAcquisitionVerbEffect :: AcquisitionVerb -> GID ObjectAcquisitionActionF -> SashaLambdaDSL Effect
  CreateContainerAcquisitionVerbEffect :: AcquisitionVerb -> GID ContainerAcquisitionActionF -> SashaLambdaDSL Effect
  CreateLocationAcquisitionVerbEffect :: AcquisitionVerb -> GID LocationAcquisitionActionF -> SashaLambdaDSL Effect
  CreateAgentAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID AgentAcquisitionActionF -> SashaLambdaDSL Effect
  CreateObjectAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID ObjectAcquisitionActionF -> SashaLambdaDSL Effect
  CreateContainerAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID ContainerAcquisitionActionF -> SashaLambdaDSL Effect
  CreateLocationAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID LocationAcquisitionActionF -> SashaLambdaDSL Effect
  CreateConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> SashaLambdaDSL Effect
  CreatePositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL Effect
  CreateNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> SashaLambdaDSL Effect
  CreateSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaLambdaDSL Effect
  CreateContainerAccessEffect :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaLambdaDSL Effect
  CreateContainerAccessVerbPhraseEffect :: ContainerAccessVerbPhrase -> GID ContainerAccessActionF -> SashaLambdaDSL Effect

  LinkFieldEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaLambdaDSL ()
  LinkFieldEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaLambdaDSL ()
  LinkFieldEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaLambdaDSL ()

  LinkEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaLambdaDSL ()
  LinkEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaLambdaDSL ()
  LinkEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaLambdaDSL ()
  LinkActionKeyToSystemEffect :: ActionEffectKey -> SystemEffectKey -> SashaLambdaDSL ()
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


declareDirectionalContainerActionGID :: DirectionalStimulusContainerActionF -> SashaLambdaDSL (GID DirectionalStimulusContainerActionF)
declareDirectionalContainerActionGID = DeclareDirectionalContainerActionGID

declareSomaticActionGID :: SomaticAccessActionF -> SashaLambdaDSL (GID SomaticAccessActionF)
declareSomaticActionGID = DeclareSomaticActionGID

-- Role-based implicit stimulus action declaration functions
declareAgentImplicitStimulusActionGID :: AgentImplicitStimulusActionF -> SashaLambdaDSL (GID AgentImplicitStimulusActionF)
declareAgentImplicitStimulusActionGID = DeclareAgentImplicitStimulusActionGID

declareLocationImplicitStimulusActionGID :: LocationImplicitStimulusActionF -> SashaLambdaDSL (GID LocationImplicitStimulusActionF)
declareLocationImplicitStimulusActionGID = DeclareLocationImplicitStimulusActionGID

-- Role-based directional stimulus action declaration functions
declareAgentDirectionalStimulusActionGID :: AgentDirectionalStimulusActionF -> SashaLambdaDSL (GID AgentDirectionalStimulusActionF)
declareAgentDirectionalStimulusActionGID = DeclareAgentDirectionalStimulusActionGID

declareObjectDirectionalStimulusActionGID :: ObjectDirectionalStimulusActionF -> SashaLambdaDSL (GID ObjectDirectionalStimulusActionF)
declareObjectDirectionalStimulusActionGID = DeclareObjectDirectionalStimulusActionGID

declareLocationDirectionalStimulusActionGID :: LocationDirectionalStimulusActionF -> SashaLambdaDSL (GID LocationDirectionalStimulusActionF)
declareLocationDirectionalStimulusActionGID = DeclareLocationDirectionalStimulusActionGID

-- Role-based directional container stimulus action declaration functions
declareAgentDirectionalContainerStimulusActionGID :: AgentDirectionalStimulusContainerActionF -> SashaLambdaDSL (GID AgentDirectionalStimulusContainerActionF)
declareAgentDirectionalContainerStimulusActionGID = DeclareAgentDirectionalContainerStimulusActionGID

declareContainerDirectionalContainerStimulusActionGID :: ContainerDirectionalStimulusContainerActionF -> SashaLambdaDSL (GID ContainerDirectionalStimulusContainerActionF)
declareContainerDirectionalContainerStimulusActionGID = DeclareContainerDirectionalContainerStimulusActionGID

declareLocationDirectionalContainerStimulusActionGID :: LocationDirectionalStimulusContainerActionF -> SashaLambdaDSL (GID LocationDirectionalStimulusContainerActionF)
declareLocationDirectionalContainerStimulusActionGID = DeclareLocationDirectionalContainerStimulusActionGID

-- Role-based acquisition action declaration functions
declareAgentAcquisitionActionGID :: AgentAcquisitionActionF -> SashaLambdaDSL (GID AgentAcquisitionActionF)
declareAgentAcquisitionActionGID = DeclareAgentAcquisitionActionGID

declareObjectAcquisitionActionGID :: ObjectAcquisitionActionF -> SashaLambdaDSL (GID ObjectAcquisitionActionF)
declareObjectAcquisitionActionGID = DeclareObjectAcquisitionActionGID

declareContainerAcquisitionActionGID :: ContainerAcquisitionActionF -> SashaLambdaDSL (GID ContainerAcquisitionActionF)
declareContainerAcquisitionActionGID = DeclareContainerAcquisitionActionGID

declareLocationAcquisitionActionGID :: LocationAcquisitionActionF -> SashaLambdaDSL (GID LocationAcquisitionActionF)
declareLocationAcquisitionActionGID = DeclareLocationAcquisitionActionGID

declareConsumptionActionGID :: ConsumptionActionF -> SashaLambdaDSL (GID ConsumptionActionF)
declareConsumptionActionGID = DeclareConsumptionActionGID

declarePosturalActionGID :: PosturalActionF -> SashaLambdaDSL (GID PosturalActionF)
declarePosturalActionGID = DeclarePosturalActionGID

declareContainerAccessActionGID :: ContainerAccessActionF -> SashaLambdaDSL (GID ContainerAccessActionF)
declareContainerAccessActionGID = DeclareContainerAccessActionGID


createDSAContainerManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
createDSAContainerManagement = CreateDSAContainerManagement

-- Role-based directional container stimulus management creation helper functions
createAgentDSAContainerManagement :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
createAgentDSAContainerManagement = CreateAgentDSAContainerManagement

createContainerDSAContainerManagement :: DirectionalStimulusVerb -> GID ContainerDirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
createContainerDSAContainerManagement = CreateContainerDSAContainerManagement

createLocationDSAContainerManagement :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusContainerActionF -> SashaLambdaDSL ActionManagement
createLocationDSAContainerManagement = CreateLocationDSAContainerManagement

createSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> SashaLambdaDSL ActionManagement
createSSAManagement = CreateSSAManagement

createSAConManagement :: SimpleAccessVerb -> GID ContainerAccessActionF -> SashaLambdaDSL ActionManagement
createSAConManagement = CreateSAConManagement


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


createDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
createDirectionalContainerStimulusEffect = CreateDirectionalContainerStimulusEffect

-- Role-based directional stimulus effect creation helper functions

createAgentImplicitStimulusEffect :: ImplicitStimulusVerb -> GID AgentImplicitStimulusActionF -> SashaLambdaDSL Effect
createAgentImplicitStimulusEffect = CreateAgentImplicitStimulusEffect

createLocationImplicitStimulusEffect :: ImplicitStimulusVerb -> GID LocationImplicitStimulusActionF -> SashaLambdaDSL Effect
createLocationImplicitStimulusEffect = CreateLocationImplicitStimulusEffect

createAgentDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusActionF -> SashaLambdaDSL Effect
createAgentDirectionalStimulusEffect = CreateAgentDirectionalStimulusEffect

createObjectDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID ObjectDirectionalStimulusActionF -> SashaLambdaDSL Effect
createObjectDirectionalStimulusEffect = CreateObjectDirectionalStimulusEffect

createLocationDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusActionF -> SashaLambdaDSL Effect
createLocationDirectionalStimulusEffect = CreateLocationDirectionalStimulusEffect

-- Role-based directional container stimulus effect creation helper functions
createAgentDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID AgentDirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
createAgentDirectionalContainerStimulusEffect = CreateAgentDirectionalContainerStimulusEffect

createContainerDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID ContainerDirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
createContainerDirectionalContainerStimulusEffect = CreateContainerDirectionalContainerStimulusEffect

createLocationDirectionalContainerStimulusEffect :: DirectionalStimulusVerb -> GID LocationDirectionalStimulusContainerActionF -> SashaLambdaDSL Effect
createLocationDirectionalContainerStimulusEffect = CreateLocationDirectionalContainerStimulusEffect

-- Role-based acquisition effect creation helper functions
createAgentAcquisitionVerbEffect :: AcquisitionVerb -> GID AgentAcquisitionActionF -> SashaLambdaDSL Effect
createAgentAcquisitionVerbEffect = CreateAgentAcquisitionVerbEffect

createObjectAcquisitionVerbEffect :: AcquisitionVerb -> GID ObjectAcquisitionActionF -> SashaLambdaDSL Effect
createObjectAcquisitionVerbEffect = CreateObjectAcquisitionVerbEffect

createContainerAcquisitionVerbEffect :: AcquisitionVerb -> GID ContainerAcquisitionActionF -> SashaLambdaDSL Effect
createContainerAcquisitionVerbEffect = CreateContainerAcquisitionVerbEffect

createLocationAcquisitionVerbEffect :: AcquisitionVerb -> GID LocationAcquisitionActionF -> SashaLambdaDSL Effect
createLocationAcquisitionVerbEffect = CreateLocationAcquisitionVerbEffect

createAgentAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID AgentAcquisitionActionF -> SashaLambdaDSL Effect
createAgentAcquisitionVerbPhraseEffect = CreateAgentAcquisitionVerbPhraseEffect

createObjectAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID ObjectAcquisitionActionF -> SashaLambdaDSL Effect
createObjectAcquisitionVerbPhraseEffect = CreateObjectAcquisitionVerbPhraseEffect

createContainerAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID ContainerAcquisitionActionF -> SashaLambdaDSL Effect
createContainerAcquisitionVerbPhraseEffect = CreateContainerAcquisitionVerbPhraseEffect

createLocationAcquisitionVerbPhraseEffect :: AcquisitionVerbPhrase -> GID LocationAcquisitionActionF -> SashaLambdaDSL Effect
createLocationAcquisitionVerbPhraseEffect = CreateLocationAcquisitionVerbPhraseEffect


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

createContainerAccessVerbPhraseEffect :: ContainerAccessVerbPhrase -> GID ContainerAccessActionF -> SashaLambdaDSL Effect
createContainerAccessVerbPhraseEffect = CreateContainerAccessVerbPhraseEffect

linkEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaLambdaDSL ()
linkEffectToObject = LinkEffectToObject

linkEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaLambdaDSL ()
linkEffectToLocation = LinkEffectToLocation

linkEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaLambdaDSL ()
linkEffectToPlayer = LinkEffectToPlayer

linkActionKeyToSystemEffect :: ActionEffectKey -> SystemEffectKey -> SashaLambdaDSL ()
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

registerTrigger :: ActionEffectKey
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

linkFieldEffectToObject :: ActionEffectKey -> GID Object -> Effect -> SashaLambdaDSL ()
linkFieldEffectToObject = LinkFieldEffectToObject

linkFieldEffectToLocation :: ActionEffectKey -> GID Location -> Effect -> SashaLambdaDSL ()
linkFieldEffectToLocation = LinkFieldEffectToLocation

linkFieldEffectToPlayer :: ActionEffectKey -> PlayerKey -> Effect -> SashaLambdaDSL ()
linkFieldEffectToPlayer = LinkFieldEffectToPlayer
