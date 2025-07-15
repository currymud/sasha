module Evaluators.General where
import qualified Data.Map.Strict
import           Data.Text                     (Text)
import           Error                         (throwLeftM, throwMaybeM)
import           GameState                     (getActionF, liftGS)
import           Model.GameState               (ActionF (ImplicitStimulusF),
                                                GameStateExceptT,
                                                Location (Location),
                                                ResolutionT)
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))
import           Model.Parser.GCase            (VerbKey (ImplicitStimulusKey))

eval :: Sentence -> GameStateExceptT (Location -> ResolutionT ())
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> GameStateExceptT (Location -> ResolutionT ())
evalImperative (StimulusVerbPhrase stimulusVerbPhrase) =
  evalStimulusVerbPhrase stimulusVerbPhrase

evalStimulusVerbPhrase :: StimulusVerbPhrase
                            -> GameStateExceptT (Location -> ResolutionT ())
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = evalImplicitStimulusVerb isv

evalImplicitStimulusVerb :: ImplicitStimulusVerb
                              -> GameStateExceptT (Location -> ResolutionT ())
evalImplicitStimulusVerb isv = pure $
  \(Location desc _ amap) -> do
      aid <- throwMaybeM errMsg $ Data.Map.Strict.lookup verbKey amap
      actionF <- liftGS $ getActionF aid  -- Even simpler!
      case actionF of
        ImplicitStimulusF res -> do
          f <- throwLeftM caseMismatch res
          f desc
  where
    verbKey :: VerbKey
    verbKey = ImplicitStimulusKey isv

    caseMismatch :: Text
    caseMismatch = "Implicit stimulus verb does not match expected type"

    errMsg :: Text
    errMsg = "Implicit stimulus verb not found"
