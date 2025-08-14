{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Locations.Bedroom.Get (get,getDenied) where
import           Control.Monad.Identity        (Identity)
import qualified Data.Map.Strict
import qualified Data.Set
import           Data.Text                     (Text)
import           GameState                     (getObjectM,
                                                getPlayerLocationGID,
                                                modifyLocationM,
                                                modifyNarration,
                                                parseAcquisitionPhrase)
import           Model.GameState               (AcquisitionActionF (AcquisitionActionF, RemovedFromF),
                                                ActionEffectMap,
                                                GameComputation,
                                                Location (_locationActionManagement, _objectSemanticMap),
                                                Object (_description),
                                                updateActionConsequence)
import           Model.Parser.Composites.Verbs (AcquisitionVerbPhrase)
import           Model.Parser.GCase            (NounKey)


getDenied :: AcquisitionActionF
getDenied = RemovedFromF (Left denied)
  where
    denied :: GameComputation Identity ()
    denied = modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "not possibe"

get :: AcquisitionActionF
get = RemovedFromF getit
  where
-- (Location -> NounKey -> GameComputation Identity ( Either (GameComputation Identity ()) (GameComputation Identity ())))
    getit :: Either (GameComputation Identity ()) (GameComputation Identity ())
    getit = Left (pure ())
