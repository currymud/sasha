module Build.GameStateGeneration.EffectRegistry where

import qualified Data.Map.Strict
import qualified Data.Set
import           Model.GameState                                         (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                          ActionEffectMap (ActionEffectMap),
                                                                          ActionKey (AcquisitionalActionKey, ConsumptionActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                                          Effect (AcquisitionPhraseEffect, AcquisitionVerbEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, PerceptionEffect, PositivePosturalEffect),
                                                                          EffectRegistry,
                                                                          PlayerKey (PlayerKeyObject))

-- Import all the effect definitions and action GIDs
import           Build.Identifiers.Actions                               (agentCanSeeGID,
                                                                          alreadyHaveRobeFGID,
                                                                          dizzyGetFGID,
                                                                          getFromChairFGID,
                                                                          getFromRobeFGID,
                                                                          openEyesGID,
                                                                          playerGetFGID,
                                                                          robeCollectedFGID,
                                                                          seeChairFGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeRobeChairGID,
                                                                          seeRobeWornGID,
                                                                          seeTableGID,
                                                                          standDeniedGID,
                                                                          standUpGID,
                                                                          takePillFGID,
                                                                          whatPillGID)

-- Import DSL-generated GIDs
import           Build.GameStateGeneration.LocationSpec.LocationGIDs     (bedroomGID)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS         (chairGID,
                                                                          mailGID,
                                                                          pillGID,
                                                                          pocketGID,
                                                                          robeGID,
                                                                          smallTableGID)
import           Build.GameStateGeneration.ObjectSpec.Objects            (getRobeAVP)
import           Data.Set                                                (Set)
import qualified Grammar.Parser.Partitions.Nouns.Objectives
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (takeCV)
import qualified Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import           Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (dsaLook)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (isaLook)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Grammar.Parser.Partitions.Verbs.PosturalVerbs           (stand)
import           Model.Parser.Atomics.Nouns                              (Objective)
import           Model.Parser.Atomics.Verbs                              (ConsumptionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase))

-- =============================================================================
-- INDIVIDUAL EFFECTS
-- =============================================================================

-- Perception and Look Effects
openEyesEffect :: Set Effect
openEyesEffect = Data.Set.fromList [ImplicitStimulusEffect isaLook agentCanSeeGID, PerceptionEffect]

pillEffect :: Effect
pillEffect = DirectionalStimulusEffect dsaLook whatPillGID

tableEffect :: Effect
tableEffect = DirectionalStimulusEffect dsaLook seeTableGID

chairLookEffect :: Effect
chairLookEffect = DirectionalStimulusEffect dsaLook seeChairFGID

mailEffect :: Effect
mailEffect = DirectionalStimulusEffect dsaLook seeMailGID

robeLookEffect :: Effect
robeLookEffect = DirectionalStimulusEffect dsaLook seeRobeChairGID

robeWornEffect :: Effect
robeWornEffect = DirectionalStimulusEffect dsaLook seeRobeWornGID

pocketWornEffect :: Effect
pocketWornEffect = DirectionalStimulusEffect dsaLook seePocketRobeWornGID

-- Acquisition Effects
getFromChairEffect :: Effect
getFromChairEffect = AcquisitionVerbEffect get getFromChairFGID

getRobeEffect :: Effect
getRobeEffect = AcquisitionPhraseEffect getRobeAVP robeCollectedFGID

getFromRobeEffect :: Effect
getFromRobeEffect = AcquisitionVerbEffect get getFromRobeFGID

enableRobeGetEffect :: Effect
enableRobeGetEffect = AcquisitionPhraseEffect getRobeAVP playerGetFGID

-- Consumption Effects
pillReachableEffect :: Effect
pillReachableEffect = ConsumptionEffect takeCV pillGID takePillFGID

pillTakeableEffect :: Effect
pillTakeableEffect = ConsumptionEffect takeCV pillGID takePillFGID

-- Postural Effects
pillCuresHeadacheEffect :: Effect
pillCuresHeadacheEffect = PositivePosturalEffect stand standUpGID

-- =============================================================================
-- BUILDER FUNCTIONS
-- =============================================================================

withActionKey :: ActionKey -> ActionEffectMap -> (ActionKey, ActionEffectMap)
withActionKey key effectMap = (key, effectMap)

withEffectMap :: [(ActionEffectKey, Set Effect)] -> ActionEffectMap
withEffectMap effects = ActionEffectMap (Data.Map.Strict.fromList effects)

withEffect :: ActionEffectKey -> Effect -> (ActionEffectKey, Set Effect)
withEffect key effect = (key, Data.Set.singleton effect)

withEffects :: ActionEffectKey -> [Effect] -> (ActionEffectKey, Set Effect)
withEffects key effects = (key, Data.Set.fromList effects)

emptyEffectMap :: ActionEffectMap
emptyEffectMap = ActionEffectMap mempty

-- =============================================================================
-- MAIN EFFECT REGISTRY
-- =============================================================================

effectRegistry :: EffectRegistry
effectRegistry = Data.Map.Strict.fromList
  [ -- Open Eyes Action - Main scene setup
    withActionKey (SomaticAccessActionKey openEyesGID) $
      withEffectMap [ (LocationKey bedroomGID, openEyesEffect)
                    , withEffect (ObjectKey pillGID) pillEffect
                    , withEffect (ObjectKey smallTableGID) tableEffect
                    , withEffects (ObjectKey chairGID) [chairLookEffect, getFromChairEffect]
                    , withEffects (ObjectKey robeGID) [robeLookEffect, getRobeEffect, getFromRobeEffect]
                    , withEffect (ObjectKey mailGID) mailEffect
                    , withEffect (PlayerKey (PlayerKeyObject robeGID)) enableRobeGetEffect
                    ]

  -- Player Get Robe Action - Equipment effects
  , withActionKey (AcquisitionalActionKey playerGetFGID) $
      withEffectMap [ withEffect (PlayerKey (PlayerKeyObject robeGID)) getRobeEffect
                    , withEffect (ObjectKey robeGID) robeWornEffect
                    , withEffect (ObjectKey pocketGID) pocketWornEffect
                    , withEffects (PlayerKey (PlayerKeyObject pillGID)) [pillReachableEffect, pillTakeableEffect]
                    ]

  -- Take Pill Action - Healing effects
  , withActionKey (ConsumptionActionKey takePillFGID) $
      withEffectMap [ withEffect (PlayerKey (PlayerKeyObject pillGID)) pillCuresHeadacheEffect ]

  -- Stand Up Action - Movement unlocked
  , withActionKey (PosturalActionKey standUpGID) emptyEffectMap

  -- Denied Actions - No effects
  , withActionKey (AcquisitionalActionKey alreadyHaveRobeFGID) emptyEffectMap
  , withActionKey (PosturalActionKey standDeniedGID) emptyEffectMap
  , withActionKey (AcquisitionalActionKey dizzyGetFGID) emptyEffectMap
  ]
