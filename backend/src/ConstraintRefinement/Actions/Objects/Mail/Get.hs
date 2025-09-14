module ConstraintRefinement.Actions.Objects.Mail.Get (getMailDeniedF,alreadyHaveMailF,getMailDizzyF) where
import           Control.Monad.Identity     (Identity)
import           Data.Text                  (Text)
import           GameState                  (modifyNarration,
                                             updateActionConsequence)
import           GameState.ActionManagement (processEffectsFromRegistry)
import           Model.Core                 (AcquisitionActionF (NotGettableF),
                                             ActionEffectKey, GameComputation)


alreadyHaveMailF :: AcquisitionActionF
alreadyHaveMailF = NotGettableF haveMail
  where
    haveMail :: ActionEffectKey -> GameComputation Identity ()
    haveMail actionEffectKey = processEffectsFromRegistry actionEffectKey
                                 >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You are already have your mail. it'll probably be important later."

getMailDeniedF :: AcquisitionActionF
getMailDeniedF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey
                               >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You can't reach it from your bed. You need to get up first."

getMailDizzyF :: AcquisitionActionF
getMailDizzyF = NotGettableF denied
  where
    denied :: ActionEffectKey -> GameComputation Identity ()
    denied actionEffectKey = processEffectsFromRegistry actionEffectKey
                               >> modifyNarration (updateActionConsequence msg)
    msg :: Text
    msg = "You stand up to go to the table, but you are still a bit dizzy and lay back down"
