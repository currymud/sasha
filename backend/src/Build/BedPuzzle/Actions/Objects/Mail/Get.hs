{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM_" #-}
module Build.BedPuzzle.Actions.Objects.Mail.Get (getMailDeniedF,alreadyHaveMailF,getMailDizzyF) where
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import           GameState              (modifyNarration)
import           Model.GameState        (AcquisitionActionF (CollectedF),
                                         GameComputation,
                                         updateActionConsequence)


alreadyHaveMailF :: AcquisitionActionF
alreadyHaveMailF = CollectedF (const haveMail)
  where
    haveMail :: Either (GameComputation Identity ()) (GameComputation Identity ())
    haveMail = Left $ modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You are already have your mail. it'll probably be important later."

getMailDeniedF :: AcquisitionActionF
getMailDeniedF = CollectedF (const denied)
  where
    denied :: Either (GameComputation Identity ()) (GameComputation Identity ())
    denied = Left $ modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You can't reach it from your bed. You need to get up first."

getMailDizzyF :: AcquisitionActionF
getMailDizzyF = CollectedF (const denied)
  where
    denied :: Either (GameComputation Identity ()) (GameComputation Identity ())
    denied = Left $ modifyNarration $ updateActionConsequence msg
    msg :: Text
    msg = "You stand up to go to the table, but you are still a bit dizzy and lay back down"
