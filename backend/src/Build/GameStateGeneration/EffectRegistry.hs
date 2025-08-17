-- Create Build/GameStateGeneration/EffectRegistry.hs

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

-- Import new DSL-generated GIDs
import           Build.GameStateGeneration.LocationSpec.LocationGIDs     (bedroomGID)
import           Build.GameStateGeneration.ObjectSpec.ObjectGIDS         (chairGID,
                                                                          mailGID,
                                                                          pillGID,
                                                                          pocketGID,
                                                                          robeGID,
                                                                          smallTableGID)
import           Data.Set                                                (Set)
import           Debug.Trace                                             (trace)
import qualified Grammar.Parser.Partitions.Nouns.Objectives
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import qualified Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Grammar.Parser.Partitions.Verbs.PosturalVerbs           (stand)
import           Model.Parser.Atomics.Nouns                              (Objective)
import           Model.Parser.Atomics.Verbs                              (ConsumptionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase))
import           TopLevel                                                (batchProcess)

-- Builder functions for effects
withActionKey :: ActionKey -> ActionEffectMap -> (ActionKey, ActionEffectMap)
withActionKey key effectMap = (key, effectMap)

withEffectMap :: [(ActionEffectKey, Set Effect)] -> ActionEffectMap
withEffectMap effects = ActionEffectMap (Data.Map.Strict.fromList effects)

withEffect :: ActionEffectKey -> Effect -> (ActionEffectKey, Set Effect)
withEffect key effect = (key, Data.Set.singleton effect)

withEffects :: ActionEffectKey -> [Effect] -> (ActionEffectKey, Set Effect)
withEffects key effects = (key, Data.Set.fromList effects)

takeCV :: ConsumptionVerb
takeCV = Grammar.Parser.Partitions.Verbs.ConsumptionVerbs.take

dirLook :: DirectionalStimulusVerb
dirLook = Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb.look

impLook :: ImplicitStimulusVerb
impLook = Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb.look
-- Effect definitions (migrated from WorldBuilder)
openEyesEffect :: Set Effect
openEyesEffect = Data.Set.fromList [ImplicitStimulusEffect impLook agentCanSeeGID, PerceptionEffect]

pillEffect :: Effect
pillEffect = DirectionalStimulusEffect dirLook whatPillGID

tableEffect :: Effect
tableEffect = DirectionalStimulusEffect dirLook seeTableGID

chairLookEffect :: Effect
chairLookEffect = DirectionalStimulusEffect dirLook seeChairFGID

getFromChairEffect :: Effect
getFromChairEffect = AcquisitionVerbEffect get getFromChairFGID

robeLookEffect :: Effect
robeLookEffect = DirectionalStimulusEffect dirLook seeRobeChairGID

mailEffect :: Effect
mailEffect = DirectionalStimulusEffect dirLook seeMailGID

robeOB :: Objective
robeOB = Grammar.Parser.Partitions.Nouns.Objectives.robe

simpleRobeOP :: ObjectPhrase
simpleRobeOP = ObjectPhrase (SimpleNounPhrase robeOB)

getRobeAVP :: AcquisitionVerbPhrase
getRobeAVP = SimpleAcquisitionVerbPhrase get simpleRobeOP

getRobeEffect :: Effect
getRobeEffect = trace ("DEBUG: robeCollectedFGID resolves to " ++ show robeCollectedFGID) $ AcquisitionPhraseEffect getRobeAVP robeCollectedFGID

getFromRobeEffect :: Effect
getFromRobeEffect = AcquisitionVerbEffect get getFromRobeFGID

robeWornEffect :: Effect
robeWornEffect = DirectionalStimulusEffect dirLook seeRobeWornGID

enableRobeGetEffect :: Effect
enableRobeGetEffect = AcquisitionPhraseEffect getRobeAVP playerGetFGID

pocketWornEffect :: Effect
pocketWornEffect = DirectionalStimulusEffect dirLook seePocketRobeWornGID

-- Pill-related effects
pillReachableEffect :: Effect
pillReachableEffect = ConsumptionEffect takeCV pillGID takePillFGID

pillTakeableEffect :: Effect
pillTakeableEffect = ConsumptionEffect takeCV pillGID takePillFGID

pillCuresHeadacheEffect :: Effect
pillCuresHeadacheEffect = PositivePosturalEffect stand standUpGID


-- Build the effect registry using DSL
effectRegistry :: EffectRegistry
effectRegistry = Data.Map.Strict.fromList
  [ withActionKey (SomaticAccessActionKey openEyesGID) $
      withEffectMap [ (LocationKey bedroomGID, openEyesEffect)
                    , withEffect (ObjectKey pillGID) pillEffect
                    , withEffect (ObjectKey smallTableGID) tableEffect
                    , withEffects (ObjectKey chairGID) [chairLookEffect, getFromChairEffect]
                    , withEffects (ObjectKey robeGID) [robeLookEffect, getRobeEffect, getFromRobeEffect]
                    , withEffect (ObjectKey mailGID) mailEffect
                    , withEffect (PlayerKey (PlayerKeyObject robeGID)) enableRobeGetEffect
                    ]

  , withActionKey (AcquisitionalActionKey playerGetFGID) $
      withEffectMap [ withEffect (PlayerKey (PlayerKeyObject robeGID)) getRobeEffect
                    , withEffect (ObjectKey robeGID) robeWornEffect
                    , withEffect (ObjectKey pocketGID) pocketWornEffect
                    , withEffects (PlayerKey (PlayerKeyObject pillGID)) [pillReachableEffect, pillTakeableEffect]
                    ]

  , withActionKey (AcquisitionalActionKey alreadyHaveRobeFGID) emptyEffectMap
  , withActionKey (PosturalActionKey standDeniedGID) emptyEffectMap
  , withActionKey (ConsumptionActionKey takePillFGID) $
      withEffectMap [ withEffect (PlayerKey (PlayerKeyObject pillGID)) pillCuresHeadacheEffect ]
  , withActionKey (PosturalActionKey standUpGID) emptyEffectMap
  , withActionKey (AcquisitionalActionKey dizzyGetFGID) emptyEffectMap
  ]

emptyEffectMap :: ActionEffectMap
emptyEffectMap = ActionEffectMap mempty

-- All the individual effect definitions would need to be moved here or imported
-- This is just showing the structure - the actual effects would be defined here:q
