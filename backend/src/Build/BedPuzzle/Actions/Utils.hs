{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Utils where
import           Control.Monad.Identity (Identity)
import           Data.Kind              (Type)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (GameComputation,
                                         updateActionConsequence)

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
