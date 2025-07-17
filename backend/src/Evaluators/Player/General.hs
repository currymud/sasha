module Evaluators.Player.General where
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Text                     (Text)
import           GameState                     (getPlayerActionF)
import           Location                      (getLocationIdM)
import           Model.GameState               (ActionF (ComputeLocation),
                                                GameStateExceptT,
                                                ResolutionT (ResolutionT))
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))
import           Model.Parser.GCase            (VerbKey (ImplicitStimulusKey))

-- The computation that fits into your game loop
type GameComputation = ResolutionT ()

eval :: Sentence -> GameStateExceptT GameComputation
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameStateExceptT GameComputation
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase

evalStimulusVerbPhrase :: StimulusVerbPhrase -> GameStateExceptT GameComputation
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = evalImplicitStimulusVerb isv

evalImplicitStimulusVerb :: ImplicitStimulusVerb -> GameStateExceptT GameComputation
evalImplicitStimulusVerb isv = do
  -- Step 1: Access the player's action map
  playerAction <- getPlayerActionF verbKey
  case playerAction of
    Left err     -> pure $ printWrong err
    Right action -> pure $ evalAction action

  where
    verbKey = ImplicitStimulusKey isv

printWrong :: Text -> GameComputation
printWrong msg =
  ResolutionT $ liftIO $ putStrLn $ "Wrong: " ++ show msg

evalAction :: ActionF -> GameComputation
evalAction (ComputeImplicitStimulusAction locf) = do
  ResolutionT getLocationIdM
