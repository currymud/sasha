module Evaluators.Player.General where
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Data                     (Data (dataTypeOf))
import           Data.Text                     (Text)
import           GameState                     (getImplicitStimulusVerbProcessor)
import           Model.GameState               (ActionF, GameComputation,
                                                ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb),
                                                ResolutionT (ResolutionT))
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))
import           Model.Parser.GCase            (VerbKey (ImplicitStimulusKey))


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
