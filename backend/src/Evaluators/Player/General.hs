module Evaluators.Player.General where
import           Control.Monad.Identity        (Identity)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Data                     (Data (dataTypeOf))
import           Data.Text                     (Text)
import           GameState                     (getImplicitStimulusVerbProcessor)
import           Model.GameState               (GameComputation,
                                                ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb))
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (DirectStimulusVerbPhrase, ImplicitStimulusVerb))

eval :: Sentence -> GameComputation Identity ()
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameComputation Identity ()
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase

evalStimulusVerbPhrase :: StimulusVerbPhrase -> GameComputation Identity ()
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = evalImplicitStimulusVerb isv
evalStimulusVerbPhrase (DirectStimulusVerbPhrase dsv _ dsp) = pure ()
evalImplicitStimulusVerb :: ImplicitStimulusVerb -> GameComputation Identity ()
evalImplicitStimulusVerb isv = do
  (ProcessImplicitStimulusVerb f) <- getImplicitStimulusVerbProcessor isv
  f isv
