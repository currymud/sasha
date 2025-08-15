{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Build.GameState where
import           Build.BedPuzzle.Actions.Objects.Mail                    (getMailAVP)
import           Build.BedPuzzle.Actions.Objects.Pill                    (takePillCVP)
import           Build.GameStateGeneration.BedroomWorldSpec              (bedroomPlayerSpec,
                                                                          bedroomWorldSpec)
import           Build.GameStateGeneration.WorldBuilder                  (buildGameStateFromSpec,
                                                                          buildWorldFromSpec)
import           Build.Identifiers.Actions                               (acquisitionActionMap,
                                                                          agentCanSeeGID,
                                                                          alreadyHaveRobeFGID,
                                                                          checkInventoryGID,
                                                                          consumptionActionMap,
                                                                          directionalStimulusActionMap,
                                                                          dizzyGetFGID,
                                                                          dsvEnabledLookGID,
                                                                          getMailDeniedFGID,
                                                                          implicitStimulusActionMap,
                                                                          isaEnabledLookGID,
                                                                          openEyesGID,
                                                                          pillTooFarFGID,
                                                                          playerGetFGID,
                                                                          posturalActionMap,
                                                                          seeChairFGID,
                                                                          seeMailGID,
                                                                          seePocketRobeWornGID,
                                                                          seeRobeChairGID,
                                                                          seeRobeWornGID,
                                                                          seeTableGID,
                                                                          somaticAccessActionMap,
                                                                          standDeniedGID,
                                                                          standUpGID,
                                                                          takePillFGID,
                                                                          whatPillGID)
import           Build.Identifiers.Locations                             (bedroomInBedGID)
import           Build.Identifiers.Objects                               (chairObjGID,
                                                                          mailObjGID,
                                                                          pillObjGID,
                                                                          pocketObjGID,
                                                                          robeObjGID,
                                                                          tableObjGID)
import           Build.World                                             (world)
import           Data.Map.Strict                                         (empty,
                                                                          fromList)
import           Data.Set                                                (Set)
import qualified Data.Set
import           Data.Text                                               (Text)
import           Evaluators.Player.General                               (eval)
import           Grammar.Parser.Partitions.Nouns.Objectives              (mail,
                                                                          pill,
                                                                          robe)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import           Grammar.Parser.Partitions.Verbs.ConsumptionVerbs        (take)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb (look)
import           Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (inventory)
import qualified Grammar.Parser.Partitions.Verbs.ImplicitStimulusVerb    (look)
import           Grammar.Parser.Partitions.Verbs.PosturalVerbs           (stand)
import qualified Grammar.Parser.Partitions.Verbs.SomaticAccessVerbs      (open)
import           Model.GameState                                         (ActionEffectKey (LocationKey, ObjectKey, PlayerKey),
                                                                          ActionEffectMap (ActionEffectMap),
                                                                          ActionKey (AcquisitionalActionKey, ConsumptionActionKey, PosturalActionKey, SomaticAccessActionKey),
                                                                          ActionKeyMap (ActionKeyMap),
                                                                          ActionManagement (AAManagementKey, CAManagementKey, DSAManagementKey, ISAManagementKey, PPManagementKey, SSAManagementKey),
                                                                          ActionManagementFunctions (ActionManagementFunctions),
                                                                          ActionMaps (ActionMaps),
                                                                          Config (Config, _actionMaps),
                                                                          Effect (AcquisitionEffect, ConsumptionEffect, DirectionalStimulusEffect, ImplicitStimulusEffect, PerceptionEffect, PositivePosturalEffect),
                                                                          EffectRegistry,
                                                                          GameState,
                                                                          Narration (..),
                                                                          Player (Player, _location, _playerActions),
                                                                          PlayerKey (PlayerKeyObject),
                                                                          World)
import qualified Model.Parser.Atomics.Nouns
import           Model.Parser.Atomics.Verbs                              (ConsumptionVerb,
                                                                          DirectionalStimulusVerb,
                                                                          ImplicitStimulusVerb)
import           Model.Parser.Composites.Nouns                           (NounPhrase (SimpleNounPhrase),
                                                                          ObjectPhrase (ObjectPhrase))
import           Model.Parser.Composites.Verbs                           (AcquisitionVerbPhrase (SimpleAcquisitionVerbPhrase),
                                                                          ConsumptionVerbPhrase)
import           Prelude                                                 hiding
                                                                         (take)
initNarration :: Narration
initNarration = Narration
  { _playerAction  = [initialAction]
  , _actionConsequence = mempty
  }

initialAction :: Text
initialAction = "Finally! Let's see how look works"


config :: Config
config = Config
  { _actionMaps = actionMaps
  }
  where
    actionMaps :: ActionMaps
    actionMaps = ActionMaps
                   implicitStimulusActionMap
                   directionalStimulusActionMap
                   somaticAccessActionMap
                   acquisitionActionMap
                   consumptionActionMap
                   posturalActionMap

gameState :: GameState
gameState = buildGameStateFromSpec bedroomWorldSpec bedroomPlayerSpec


