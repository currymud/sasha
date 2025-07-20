module Evaluators.Player.General where
import           GameState                     (getImplicitStimulusVerbProcessor)
import           Model.GameState               (GameComputation,
                                                ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb))
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))


eval :: Sentence -> GameComputation
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameComputation
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase

evalStimulusVerbPhrase :: StimulusVerbPhrase -> GameComputation
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = evalImplicitStimulusVerb isv

evalImplicitStimulusVerb :: ImplicitStimulusVerb -> GameComputation
evalImplicitStimulusVerb isv = do
  (ProcessImplicitStimulusVerb f) <- getImplicitStimulusVerbProcessor isv
  f isv
