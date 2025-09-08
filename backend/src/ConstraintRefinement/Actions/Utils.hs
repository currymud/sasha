{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module ConstraintRefinement.Actions.Utils where
import           Control.Monad.Identity (Identity)
import           Data.Kind              (Type)
import           Data.Text              (Text)
import           GameState              (modifyNarration,
                                         updateActionConsequence)
import           Model.Core             (GameComputation)

type AcquisitionError :: Type
data AcquisitionError
  = ObjectNotFound Text
  | ObjectNotGettable Text
  | ContainerMissingAction Text
  | InvalidActionType Text
  | SpatialValidationFailed Text

handleAcquisitionError :: AcquisitionError -> GameComputation Identity ()
handleAcquisitionError err = modifyNarration $ updateActionConsequence $ case err of
  ObjectNotFound msg          -> msg
  ObjectNotGettable msg       -> msg
  ContainerMissingAction msg  -> msg
  InvalidActionType msg       -> msg
  SpatialValidationFailed msg -> msg
