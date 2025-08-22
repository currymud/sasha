module Evaluators.Player.General where
import           Actions.Administrative                (manageAdministration)
import           Actions.Consume                       (manageConsumptionProcess)
import           Actions.Get.Acquisition.Get           (manageAcquisitionProcess)
import           Actions.Manipulate.SomaticAccess.Open (manageSomaticAccessProcess)
import           Actions.Movement.Postural.SitStand    (managePosturalProcess)
import           Actions.Percieve.Look                 (manageDirectionalStimulusProcess,
                                                        manageImplicitStimulusProcess)
import           Control.Monad.Identity                (Identity)
import           Model.GameState                       (GameComputation)
import           Model.Parser                          (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs            (AdministrativeVerb (AdministrativeVerb))
import           Model.Parser.Composites.Verbs         (AcquisitionVerbPhrase (AcquisitionVerbPhrase),
                                                        Imperative (AcquisitionVerbPhrase', Administrative, ConsumptionVerbPhrase', PosturalVerbPhrase, StimulusVerbPhrase),
                                                        PosturalVerbPhrase,
                                                        StimulusVerbPhrase (DirectStimulusVerbPhrase, ImplicitStimulusVerb, SomaticStimulusVerbPhrase))
eval :: Sentence -> GameComputation Identity ()
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameComputation Identity ()
evalImperative (Administrative av) = manageAdministration av
evalImperative (ConsumptionVerbPhrase' consumptionVerbPhrase) =  -- NEW
  manageConsumptionProcess consumptionVerbPhrase
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase
evalImperative (AcquisitionVerbPhrase' acquisitionVerbPhrase) =
  evalAcquisitionVerbPhrase acquisitionVerbPhrase
evalImperative (PosturalVerbPhrase postureVerbPhrase) = evalPosturalVerbPhrase postureVerbPhrase -- Posture verbs are not implemented yet

evalStimulusVerbPhrase :: StimulusVerbPhrase -> GameComputation Identity ()
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = manageImplicitStimulusProcess isv
evalStimulusVerbPhrase (DirectStimulusVerbPhrase dsv _ dsp) = manageDirectionalStimulusProcess dsv dsp
evalStimulusVerbPhrase (SomaticStimulusVerbPhrase sav _) = manageSomaticAccessProcess sav

evalAcquisitionVerbPhrase :: AcquisitionVerbPhrase -> GameComputation Identity ()
evalAcquisitionVerbPhrase = manageAcquisitionProcess

evalPosturalVerbPhrase :: PosturalVerbPhrase -> GameComputation Identity ()
evalPosturalVerbPhrase = managePosturalProcess
