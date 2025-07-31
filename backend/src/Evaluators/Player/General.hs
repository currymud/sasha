module Evaluators.Player.General where
import           Actions.Manipulate.SomaticAccess.Open (manageSomaticAccessProcess)
import           Actions.Percieve.Look                 (manageDirectionalStimulusProcess,
                                                        manageImplicitStimulusProcess)
import           Build.Identifiers.Actions             (agentCanSeeGID)
import           Control.Monad.Identity                (Identity)
import           Model.GameState                       (GameComputation)
import           Model.Parser                          (Sentence (Imperative))
import           Model.Parser.Composites.Verbs         (Imperative (StimulusVerbPhrase),
                                                        StimulusVerbPhrase (DirectStimulusVerbPhrase, ImplicitStimulusVerb, SomaticStimulusVerbPhrase))

eval :: Sentence -> GameComputation Identity ()
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameComputation Identity ()
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase

evalStimulusVerbPhrase :: StimulusVerbPhrase -> GameComputation Identity ()
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = manageImplicitStimulusProcess isv
evalStimulusVerbPhrase (DirectStimulusVerbPhrase dsv _ dsp) = manageDirectionalStimulusProcess dsv dsp
evalStimulusVerbPhrase (SomaticStimulusVerbPhrase sav snp) = manageSomaticAccessProcess sav agentCanSeeGID
