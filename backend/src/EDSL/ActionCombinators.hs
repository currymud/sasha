module EDSL.ActionCombinators where

import           Control.Monad.Identity                                  (Identity)
import           Data.Text                                               (Text)
import           GameState                                               (modifyNarration,
                                                                          updateActionConsequence)
import           Model.Core                                              (AcquisitionActionF (NotGettableF),
                                                                          ActionManagement (AVManagementKey, DSAContainerManagementKey, DSAManagementKey),
                                                                          DirectionalStimulusActionF (CannotSeeF),
                                                                          DirectionalStimulusContainerActionF (CannotSeeInF),
                                                                          GameComputation,
                                                                          Object)
import           Model.GID                                               (GID)

-- Import existing action constructors
import           ConstraintRefinement.Actions.Objects.Get.Constructors   (getObjectF)
import           ConstraintRefinement.Actions.Player.Look                (lookAtF,
                                                                          lookInF)

-- Import verbs (disambiguated)
import           Grammar.Parser.Partitions.Verbs.AcquisitionVerbs        (get)
import qualified Grammar.Parser.Partitions.Verbs.DirectionalStimulusVerb as DSV
import           Model.GameState.GameStateDSL                            (WorldDSL,
                                                                          declareAcquisitionActionGID,
                                                                          declareDirectionalContainerActionGID,
                                                                          declareDirectionalStimulusActionGID,
                                                                          withObjectBehavior)

-- Helper
addNarration :: Text -> GameComputation Identity ()
addNarration msg = modifyNarration (updateActionConsequence msg)

-- ============================================================================
-- LOOK COMBINATORS
-- ============================================================================

-- Look AT an object (uses existing lookAtF)
lookable :: GID Object -> Object -> WorldDSL Object
lookable objGID obj = do
  actionGID <- declareDirectionalStimulusActionGID (lookAtF objGID)
  withObjectBehavior obj (DSAManagementKey DSV.look actionGID)

-- Can't look at
notLookable :: Text -> Object -> WorldDSL Object
notLookable denialMsg obj = do
  let action = CannotSeeF (addNarration denialMsg)
  actionGID <- declareDirectionalStimulusActionGID action
  withObjectBehavior obj (DSAManagementKey DSV.look actionGID)

-- Look IN a container (uses existing lookInF)
lookInside :: GID Object -> Text -> Object -> WorldDSL Object
lookInside containerGID flavorText obj = do
  actionGID <- declareDirectionalContainerActionGID (lookInF containerGID flavorText)
  withObjectBehavior obj (DSAContainerManagementKey DSV.look actionGID)

-- Can't look in
cannotLookInside :: Text -> Object -> WorldDSL Object
cannotLookInside denialMsg obj = do
  let action = CannotSeeInF (addNarration denialMsg)
  actionGID <- declareDirectionalContainerActionGID action
  withObjectBehavior obj (DSAContainerManagementKey DSV.look actionGID)

-- ============================================================================
-- GET COMBINATORS
-- ============================================================================

-- Can get object (uses existing getObjectF)
gettable :: GID Object -> Object -> WorldDSL Object
gettable objGID obj = do
  actionGID <- declareAcquisitionActionGID (getObjectF objGID)
  withObjectBehavior obj (AVManagementKey get actionGID)

-- Can't get
notGettable :: Text -> Object -> WorldDSL Object
notGettable denialMsg obj = do
  let action = NotGettableF (addNarration denialMsg)
  actionGID <- declareAcquisitionActionGID action
  withObjectBehavior obj (AVManagementKey get actionGID)
