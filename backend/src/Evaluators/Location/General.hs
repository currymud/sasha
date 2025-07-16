module Evaluators.Location.General where
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import qualified Data.Map.Strict
import           Data.Text                     (Text)
import           Error                         (throwLeftM, throwMaybeM)
import           GameState                     (getActionF, liftGS)
import           Model.GameState               (ActionF (ComputeAction),
                                                GameStateExceptT,
                                                Location (Location),
                                                ProcessSentence (ImplicitStimulusF),
                                                ResolutionT (ResolutionT))
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))
import           Model.Parser.GCase            (VerbKey (ImplicitStimulusKey))

eval :: Sentence -> GameStateExceptT (Location -> Either (ResolutionT ()) (ResolutionT ()))

eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameStateExceptT (Location -> Either (ResolutionT ()) (ResolutionT ()))


evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase

evalStimulusVerbPhrase :: StimulusVerbPhrase
                            -> GameStateExceptT (Location -> Either (ResolutionT ()) (ResolutionT ()))


evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = evalImplicitStimulusVerb isv

evalImplicitStimulusVerb :: ImplicitStimulusVerb
                              -> GameStateExceptT (Location -> Either (ResolutionT ()) (ResolutionT ()))
evalImplicitStimulusVerb isv = pure $
  \(Location desc _ amap) -> do
      case Data.Map.Strict.lookup verbKey amap of
        Nothing -> Left $ ResolutionT $ liftIO $ print errMsg
        Just aid -> do
          actionF :: ActionF <- getActionF aid
          Left $ ResolutionT $ liftIO $ print "placeholder until I figure it out"
  {-
          case actionF of
            (ComputeAction (ImplicitStimulusF res)) -> do
              f <- throwLeftM caseMismatch res
              Right $ f desc
              -}
  where
    verbKey :: VerbKey
    verbKey = ImplicitStimulusKey isv

    caseMismatch :: Text
    caseMismatch = "Implicit stimulus verb does not match expected type"

    errMsg :: Text
    errMsg = "Implicit stimulus verb not found"
