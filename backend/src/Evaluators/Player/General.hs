module Evaluators.Player.General where
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Text                     (Text)
import           Model.GameState               (ActionF, GameComputation,
                                                ResolutionT (ResolutionT))
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))
import           Model.Parser.GCase            (VerbKey (ImplicitStimulusKey))


eval :: Sentence -> GameComputation
eval (Imperative imperative) = undefined -- evalImperative imperative
  {-
evalImperative :: Imperative -> GameComputation
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase

evalStimulusVerbPhrase :: StimulusVerbPhrase -> GameComputation
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = evalImplicitStimulusVerb isv

evalImplicitStimulusVerb :: ImplicitStimulusVerb -> GameComputation
evalImplicitStimulusVerb isv = do
  -- Step 1: Access the player's action map
  playerAction <- getPlayerActionF verbKey
  case playerAction of
    Left err     -> printWrong err
    Right action -> evalActionF action
  where
    verbKey = ImplicitStimulusKey isv

evalActionF :: ActionF -> GameComputation
evalActionF _ =
  pure $ ResolutionT $ liftIO (print "Evaluating action")
-}

printWrong :: Text -> GameComputation
printWrong msg =
  pure $ ResolutionT $ liftIO $ print ("Wrong: " <> show msg)

