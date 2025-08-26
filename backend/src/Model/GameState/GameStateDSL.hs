{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.GameState.GameStateDSL where

import           Control.Monad                 (foldM)
import           Control.Monad.Identity        (Identity)
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectKey,
                                                ActionEffectMap,
                                                ActionManagement,
                                                ActionManagementFunctions,
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
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
                                                SomaticAccessVerb)
import           Model.Parser.Composites.Nouns (DirectionalStimulusNounPhrase,
                                                NounPhrase)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase,
                                                ConsumptionVerbPhrase)
import           Model.Parser.GCase            (NounKey)

-- | World building DSL
type WorldDSL :: Type -> Type
data WorldDSL :: Type -> Type where
  -- Pure values
  Pure :: a -> WorldDSL a

  -- Functor operations
  Map :: (a -> b) -> WorldDSL a -> WorldDSL b

  -- Applicative operations
  Apply :: WorldDSL (a -> b) -> WorldDSL a -> WorldDSL b
  Sequence :: WorldDSL a -> WorldDSL b -> WorldDSL b

  -- Monadic operations
  Bind :: WorldDSL a -> (a -> WorldDSL b) -> WorldDSL b

  -- GID Declaration constructors
  DeclareObjectGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Object)
  DeclareObjectiveGID :: GID Object -> NounPhrase Objective -> WorldDSL ()
  DeclareConsumableGID :: GID Object -> NounPhrase Consumable -> WorldDSL ()
  DeclareContainerGID :: GID Object -> NounPhrase Container -> WorldDSL ()
  DeclareLocationGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Location)

  WithShortName :: Text -> Object -> WorldDSL Object
  WithDescription :: Text -> Object -> WorldDSL Object
  WithDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> WorldDSL Object
  WithTitle :: Text -> Location -> WorldDSL Location
  -- Registration - now take direct values

  -- ActionManagement construction - returns individual management keys
  CreateISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL ActionManagement
  CreateDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL ActionManagement
  CreateSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL ActionManagement
  CreateAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL ActionManagement
  CreateAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL ActionManagement
  CreateCAManagement :: ConsumptionVerbPhrase -> GID ConsumptionActionF -> WorldDSL ActionManagement
  CreatePPManagement :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement
  CreateNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement

  SetPerceptionMap :: [(DirectionalStimulusNounPhrase, [GID Object])] -> WorldDSL ()
  SetEvaluator :: Evaluator -> WorldDSL ()
  SetInitialNarration :: Text -> WorldDSL ()
  -- Player management - now take direct values
  WithPlayerLocation :: Player -> GID Location -> WorldDSL Player -- NEW: Clean player location setting

  WithObjectBehavior :: Object -> ActionManagement -> WorldDSL Object
  WithLocationBehavior :: Location -> ActionManagement -> WorldDSL Location
  WithPlayerBehavior :: Player -> ActionManagement -> WorldDSL Player

  -- FieldEffect management - NEW: Field effect constructors
  UpdateShortName :: Text -> GID Object -> WorldDSL Effect
  UpdateDescription :: Text -> GID Object -> WorldDSL Effect
  UpdateTitle :: Text -> GID Location -> WorldDSL Effect
  UpdateLocation :: GID Location -> PlayerKey -> WorldDSL Effect

-- Map registration constructors
  RegisterObject :: GID Object -> WorldDSL Object -> WorldDSL ()
  RegisterLocation :: GID Location -> WorldDSL Location -> WorldDSL ()
  RegisterPlayer :: Player -> WorldDSL ()
  RegisterSpatial :: GID Object -> SpatialRelationship -> WorldDSL ()
  RegisterObjectToLocation :: GID Location -> GID Object -> NounKey -> WorldDSL ()
  RegisterSystemEffect :: SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> WorldDSL ()
  -- In DSL
  RegisterTrigger :: EffectActionKey -> SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> WorldDSL ()
  -- Effect management
  CreateImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL Effect
  CreateDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL Effect
  CreateAcquisitionVerbEffect :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL Effect
  CreateAcquisitionPhraseEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL Effect
  CreateConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> WorldDSL Effect
  CreatePositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
  CreateNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
  CreateSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL Effect

-- Add these after the existing LinkEffectTo* constructors:
  LinkFieldEffectToObject :: EffectActionKey -> GID Object -> Effect -> WorldDSL ()
  LinkFieldEffectToLocation :: EffectActionKey -> GID Location -> Effect -> WorldDSL ()
  LinkFieldEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> WorldDSL ()

  LinkEffectToObject :: EffectActionKey -> GID Object -> Effect -> WorldDSL ()
  LinkEffectToLocation :: EffectActionKey -> GID Location -> Effect -> WorldDSL ()
  LinkEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> WorldDSL ()
  LinkActionKeyToSystemEffect :: EffectActionKey -> SystemEffectKey -> WorldDSL ()
  DisplayVisibleObjects :: WorldDSL (GameComputation Identity ())
  -- Final assembly
  FinalizeGameState :: WorldDSL GameState

-- Instances
instance Functor WorldDSL where
  fmap = Map

instance Applicative WorldDSL where
  pure = Pure
  (<*>) = Apply

instance Monad WorldDSL where
   return = pure
   (>>=) = Bind

declareObjectGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Object)
declareObjectGID  = DeclareObjectGID

declareObjectiveGID :: GID Object -> NounPhrase Objective -> WorldDSL ()
declareObjectiveGID = DeclareObjectiveGID

declareConsumableGID :: GID Object -> NounPhrase Consumable -> WorldDSL ()
declareConsumableGID = DeclareConsumableGID

declareContainerGID :: GID Object -> NounPhrase Container -> WorldDSL ()
declareContainerGID = DeclareContainerGID

declareLocationGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Location)
declareLocationGID = DeclareLocationGID

-- ActionManagement construction - FIXED: now returns ActionManagementFunctions
createISAManagement :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL ActionManagement
createISAManagement = CreateISAManagement

createDSAManagement :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL ActionManagement
createDSAManagement = CreateDSAManagement

createSSAManagement :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL ActionManagement
createSSAManagement = CreateSSAManagement

createAAManagement :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL ActionManagement
createAAManagement = CreateAAManagement

createAVManagement :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL ActionManagement
createAVManagement = CreateAVManagement

createCAManagement :: ConsumptionVerbPhrase -> GID ConsumptionActionF -> WorldDSL ActionManagement
createCAManagement = CreateCAManagement

createPPManagement :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement
createPPManagement = CreatePPManagement

createNPManagement :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL ActionManagement
createNPManagement = CreateNPManagement


-- NEW: Clean player location setting
withPlayerLocation :: Player -> GID Location ->  WorldDSL Player
withPlayerLocation = WithPlayerLocation


withObjectBehavior :: Object -> ActionManagement -> WorldDSL Object
withObjectBehavior = WithObjectBehavior

withLocationBehavior :: Location -> ActionManagement -> WorldDSL Location
withLocationBehavior = WithLocationBehavior

withPlayerBehavior :: Player -> ActionManagement -> WorldDSL Player
withPlayerBehavior = WithPlayerBehavior

withPlayerBehaviors :: Player -> [ActionManagement] -> WorldDSL Player
withPlayerBehaviors = foldM withPlayerBehavior

-- Effect creation - now clean
createImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL Effect
createImplicitStimulusEffect = CreateImplicitStimulusEffect

createDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL Effect
createDirectionalStimulusEffect = CreateDirectionalStimulusEffect

createAcquisitionPhraseEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL Effect
createAcquisitionPhraseEffect = CreateAcquisitionPhraseEffect

createAcquisitionVerbEffect :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL Effect
createAcquisitionVerbEffect = CreateAcquisitionVerbEffect

createConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> WorldDSL Effect
createConsumptionEffect = CreateConsumptionEffect

createPositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
createPositivePosturalEffect = CreatePositivePosturalEffect

createNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
createNegativePosturalEffect = CreateNegativePosturalEffect

createSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL Effect
createSomaticAccessEffect = CreateSomaticAccessEffect

linkEffectToObject :: EffectActionKey -> GID Object -> Effect -> WorldDSL ()
linkEffectToObject = LinkEffectToObject

linkEffectToLocation :: EffectActionKey -> GID Location -> Effect -> WorldDSL ()
linkEffectToLocation = LinkEffectToLocation

linkEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> WorldDSL ()
linkEffectToPlayer = LinkEffectToPlayer

linkActionKeyToSystemEffect :: EffectActionKey -> SystemEffectKey -> WorldDSL ()
linkActionKeyToSystemEffect = LinkActionKeyToSystemEffect

finalizeGameState :: WorldDSL GameState
finalizeGameState = FinalizeGameState

-- Convenience functions
buildSequentially :: WorldDSL a -> WorldDSL b -> WorldDSL b
buildSequentially = Sequence


setEvaluator :: Evaluator -> WorldDSL ()
setEvaluator = SetEvaluator

setInitialNarration :: Text -> WorldDSL ()
setInitialNarration = SetInitialNarration

setPerceptionMap :: [(DirectionalStimulusNounPhrase, [GID Object])] -> WorldDSL ()
setPerceptionMap = SetPerceptionMap

-- Object field setters
withShortName :: Text -> Object -> WorldDSL Object
withShortName = WithShortName

withDescription :: Text -> Object -> WorldDSL Object
withDescription = WithDescription

withDescriptives :: [NounPhrase DirectionalStimulus] -> Object -> WorldDSL Object
withDescriptives = WithDescriptives

-- Location field setters
withTitle :: Text -> Location -> WorldDSL Location
withTitle = WithTitle

-- Replace existing functions:
registerObject :: GID Object -> WorldDSL Object -> WorldDSL ()
registerObject = RegisterObject

registerLocation :: GID Location -> WorldDSL Location -> WorldDSL ()
registerLocation = RegisterLocation

registerPlayer :: Player -> WorldDSL ()
registerPlayer = RegisterPlayer

registerSpatial :: GID Object -> SpatialRelationship -> WorldDSL ()
registerSpatial = RegisterSpatial

registerObjectToLocation :: GID Location -> GID Object -> NounKey -> WorldDSL ()
registerObjectToLocation = RegisterObjectToLocation

registerSystemEffect :: SystemEffectKey -> GID SystemEffect -> SystemEffectConfig -> WorldDSL ()
registerSystemEffect = RegisterSystemEffect

-- In DSL
registerTrigger :: EffectActionKey
                     -> SystemEffectKey
                     -> GID SystemEffect
                     -> SystemEffectConfig
                     -> WorldDSL ()
registerTrigger = RegisterTrigger

displayVisibleObjects :: WorldDSL (GameComputation Identity ())
displayVisibleObjects = DisplayVisibleObjects

-- FieldEffect convenience functions - parallel to effect functions
updateShortName :: Text -> GID Object -> WorldDSL Effect
updateShortName = UpdateShortName

updateDescription :: Text -> GID Object -> WorldDSL Effect
updateDescription = UpdateDescription

updateTitle :: Text -> GID Location -> WorldDSL Effect
updateTitle = UpdateTitle

updateLocation :: GID Location -> PlayerKey -> WorldDSL Effect
updateLocation = UpdateLocation

linkFieldEffectToObject :: EffectActionKey -> GID Object -> Effect -> WorldDSL ()
linkFieldEffectToObject = LinkFieldEffectToObject

linkFieldEffectToLocation :: EffectActionKey -> GID Location -> Effect -> WorldDSL ()
linkFieldEffectToLocation = LinkFieldEffectToLocation

linkFieldEffectToPlayer :: EffectActionKey -> PlayerKey -> Effect -> WorldDSL ()
linkFieldEffectToPlayer = LinkFieldEffectToPlayer
