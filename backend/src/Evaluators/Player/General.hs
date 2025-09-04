module Evaluators.Player.General where
import           Actions.Administrative                (manageAdministration)
import           Actions.Consume                       (manageConsumptionProcess)
import           Actions.Get.Acquisition.Get           (manageAcquisitionProcess)
import           Actions.Manipulate.SomaticAccess.Open (manageSomaticAccessProcess)
import           Actions.Movement.Postural.SitStand    (managePosturalProcess)
import           Actions.Percieve.Look                 (manageContainerDirectionalStimulusProcess,
                                                        manageDirectionalStimulusProcess,
                                                        manageImplicitStimulusProcess)
import           Control.Monad.Identity                (Identity)
import           Model.GameState                       (GameComputation)
import           Model.Parser                          (Sentence (Imperative))
import           Model.Parser.Composites.Verbs         (AccessVerbPhrase,
                                                        AcquisitionVerbPhrase,
                                                        Imperative (AccessVerbPhrase', AcquisitionVerbPhrase', Administrative, ConsumptionVerbPhrase', PosturalVerbPhrase, StimulusVerbPhrase),
                                                        PosturalVerbPhrase,
                                                        StimulusVerbPhrase (DirectStimulusVerbPhrase, DirectionalStimulusContainmentPhrase, ImplicitStimulusVerb, SomaticStimulusVerbPhrase))
eval :: Sentence -> GameComputation Identity ()
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameComputation Identity ()
evalImperative (Administrative av) = manageAdministration av
evalImperative (AccessVerbPhrase' avp) =  pure () -- manageSomaticAccessProcess avp
evalImperative (ConsumptionVerbPhrase' consumptionVerbPhrase) =  -- NEW
  manageConsumptionProcess consumptionVerbPhrase
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase
evalImperative (AcquisitionVerbPhrase' acquisitionVerbPhrase) =
  evalAcquisitionVerbPhrase acquisitionVerbPhrase
evalImperative (PosturalVerbPhrase postureVerbPhrase) = evalPosturalVerbPhrase postureVerbPhrase -- Posture verbs are not implemented yet

evalStimulusVerbPhrase :: StimulusVerbPhrase -> GameComputation Identity ()
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = manageImplicitStimulusProcess isv
evalStimulusVerbPhrase (DirectStimulusVerbPhrase dsv dsp) = manageDirectionalStimulusProcess dsv dsp
evalStimulusVerbPhrase (DirectionalStimulusContainmentPhrase dsv cp) = manageContainerDirectionalStimulusProcess dsv cp
evalStimulusVerbPhrase (SomaticStimulusVerbPhrase sav _) = manageSomaticAccessProcess sav

evalAcquisitionVerbPhrase :: AcquisitionVerbPhrase -> GameComputation Identity ()
evalAcquisitionVerbPhrase = manageAcquisitionProcess

evalPosturalVerbPhrase :: PosturalVerbPhrase -> GameComputation Identity ()
evalPosturalVerbPhrase = managePosturalProcess
