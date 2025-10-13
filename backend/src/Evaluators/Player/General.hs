module Evaluators.Player.General where
import           ActionDiscovery.Administrative                  (manageAdministration)
import           ActionDiscovery.Consume                         (manageConsumptionProcess)
import           ActionDiscovery.Get.Acquisition.Get             (manageAcquisitionProcess, manageAcquisitionProcessRoleBased)
import           ActionDiscovery.Manipulate.ContainerAccess.Open (manageContainerAccessProcess)
import           ActionDiscovery.Manipulate.SomaticAccess.Open   (manageSomaticAccessProcess)
import           ActionDiscovery.Movement.Postural.SitStand      (managePosturalProcess)
import           ActionDiscovery.Percieve.Look                   (manageContainerDirectionalStimulusProcess,
                                                                  manageDirectionalStimulusProcess,
                                                                  manageImplicitStimulusProcess)
import           Control.Monad.Identity                          (Identity)
import           Model.Core                                      (GameComputation)
import           Model.Parser                                    (Sentence (Imperative))
import           Model.Parser.Composites.Verbs                   (AcquisitionVerbPhrase,
                                                                  Imperative (AcquisitionVerbPhrase', Administrative, ConsumptionVerbPhrase', ContainerAccessVerbPhrase', PosturalVerbPhrase, StimulusVerbPhrase),
                                                                  PosturalVerbPhrase,
                                                                  StimulusVerbPhrase (DirectStimulusVerbPhrase, DirectionalStimulusContainmentPhrase, ImplicitStimulusVerb, SomaticStimulusVerbPhrase))
eval :: Sentence -> GameComputation Identity ()
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameComputation Identity ()
evalImperative (Administrative av) = manageAdministration av
evalImperative (ContainerAccessVerbPhrase' avp) =  manageContainerAccessProcess avp
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
-- Note: Can switch to manageAcquisitionProcessRoleBased when ready to use role-based system

evalPosturalVerbPhrase :: PosturalVerbPhrase -> GameComputation Identity ()
evalPosturalVerbPhrase = managePosturalProcess
