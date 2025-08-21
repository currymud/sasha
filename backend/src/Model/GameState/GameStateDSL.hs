{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.GameState.GameStateDSL where

import           Control.Monad                 (foldM)
import           Control.Monad.Identity        (Identity)
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Model.GameState               (AcquisitionActionF,
                                                ActionEffectMap, ActionKey,
                                                ActionManagement,
                                                ActionManagementFunctions,
                                                ConsumptionActionF,
                                                DirectionalStimulusActionF,
                                                Effect, Evaluator,
                                                GameComputation, GameState,
                                                ImplicitStimulusActionF,
                                                Location, Object, Player,
                                                PlayerKey, PosturalActionF,
                                                SomaticAccessActionF,
                                                SpatialRelationship,
                                                SystemEffect, SystemEffectKey)
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
  DeclareObjectiveGID :: NounPhrase Objective -> WorldDSL (GID Object)
  DeclareConsumableGID :: NounPhrase Consumable -> WorldDSL (GID Object)
  DeclareContainerGID :: NounPhrase Container -> WorldDSL (GID Object)
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

-- Map registration constructors
  RegisterObject :: GID Object -> WorldDSL Object -> WorldDSL ()
  RegisterLocation :: GID Location -> WorldDSL Location -> WorldDSL ()
  RegisterPlayer :: Player -> WorldDSL ()
  RegisterSpatial :: GID Object -> SpatialRelationship -> WorldDSL ()
  RegisterObjectToLocation :: GID Location -> GID Object -> NounKey -> WorldDSL ()

  -- Effect management
  CreateImplicitStimulusEffect :: ImplicitStimulusVerb -> GID ImplicitStimulusActionF -> WorldDSL Effect
  CreateDirectionalStimulusEffect :: DirectionalStimulusVerb -> GID DirectionalStimulusActionF -> WorldDSL Effect
  CreateAcquisitionVerbEffect :: AcquisitionVerb -> GID AcquisitionActionF -> WorldDSL Effect
  CreateAcquisitionPhraseEffect :: AcquisitionVerbPhrase -> GID AcquisitionActionF -> WorldDSL Effect
  CreateConsumptionEffect :: ConsumptionVerb -> GID Object -> GID ConsumptionActionF -> WorldDSL Effect
  CreatePositivePosturalEffect :: PositivePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
  CreateNegativePosturalEffect :: NegativePosturalVerb -> GID PosturalActionF -> WorldDSL Effect
  CreateSomaticAccessEffect :: SomaticAccessVerb -> GID SomaticAccessActionF -> WorldDSL Effect

  LinkEffectToObject :: GID Object -> Effect -> WorldDSL ()
  LinkEffectToLocation :: GID Location -> Effect -> WorldDSL ()
  LinkEffectToPlayer :: PlayerKey -> Effect -> WorldDSL ()
  LinkSystemEffectToAction :: ActionKey -> SystemEffect -> WorldDSL ()
  LinkActionKeyToSystemEffect :: ActionKey -> SystemEffectKey -> WorldDSL ()
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

-- Smart constructors - now clean without pure wrapping
declareObjectGID :: NounPhrase DirectionalStimulus -> WorldDSL (GID Object)
declareObjectGID = DeclareObjectGID

declareObjectiveGID :: NounPhrase Objective -> WorldDSL (GID Object)
declareObjectiveGID = DeclareObjectiveGID

declareConsumableGID :: NounPhrase Consumable -> WorldDSL (GID Object)
declareConsumableGID = DeclareConsumableGID

declareContainerGID :: NounPhrase Container -> WorldDSL (GID Object)
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

linkSystemEffectToAction :: ActionKey -> SystemEffect -> WorldDSL ()
linkSystemEffectToAction = LinkSystemEffectToAction

linkEffectToObject :: GID Object -> Effect -> WorldDSL ()
linkEffectToObject = LinkEffectToObject

linkEffectToLocation :: GID Location -> Effect -> WorldDSL ()
linkEffectToLocation = LinkEffectToLocation

linkEffectToPlayer :: PlayerKey -> Effect -> WorldDSL ()
linkEffectToPlayer = LinkEffectToPlayer

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

displayVisibleObjects :: WorldDSL (GameComputation Identity ())
displayVisibleObjects = DisplayVisibleObjects
