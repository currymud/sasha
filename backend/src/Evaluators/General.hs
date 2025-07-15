module Evaluators.General where
import qualified Data.Map.Strict
import           Data.Text                     (Text)
import           Error                         (throwLeftM, throwMaybeM)
import           GameState                     (getActionF)
import           GHC.TypeError                 (ErrorMessage (Text))
import           Location                      (getLocation)
import           Model.GameState               (ActionF (ImplicitStimulusF),
                                                GameStateExceptT,
                                                Location (Location),
                                                ResolutionT (ResolutionT))
import           Model.GID                     (GID)
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))
import           Model.Parser.GCase            (VerbKey (ImplicitStimulusKey),
                                                mkVerbKey)
import           Relude.String.Conversion      (toText)

  {-
eval :: Sentence -> (Location -> ActionF ResolutionF)
eval (Imperative imperative) = evalImperative imperative

evalImperative :: Imperative -> (Location -> ActionF ResolutionF)
evalImperative imperative =
  let vkey = mkVerbKey imperative
  in case imperative of
     StimulusVerbPhrase stimulusVerbPhrase -> evalStimulusVerbPhrase stimulusVerbPhrase vkey
     _ -> (\lid -> ImplicitStimulusF $ Left $ ResolutionF $ pure ())
-}
  {-
evalStimulusVerbPhrase :: StimulusVerbPhrase
                            -> (VerbKey -> Location -> ResolutionF)
evalStimulusVerbPhrase (ImplicitStimulusVerb isv) = evalImplicitStimulusVerb isv
-}
{-
evalImplicitStimulusVerb :: ImplicitStimulusVerb
                              -> GameStateExceptT (Location -> ResolutionT ())
evalImplicitStimulusVerb isv =
  pure $ \(Location desc _ amap) -> do
      aid <- throwMaybeM errMsg $ Data.Map.Strict.lookup verbKey amap
      actionF <- getActionF aid
      case actionF of
        ImplicitStimulusF res -> do
          f <- throwLeftM caseMismatch res
          f desc
  where
    verbKey :: VerbKey
    verbKey = ImplicitStimulusKey isv
    caseMismatch :: Text
    caseMismatch = "Implicit stimulus verb " <> toText isv <> " does not match expected type"
    errMsg :: Text
    errMsg = "Implicit stimulus verb " <> toText isv <> " not found"
    -}

getActionM :: ImplicitStimulusVerb -> GameStateExceptT (Location -> ResolutionT ())
getActionM  verbKey = pure $
  \(Location desc _ amap) -> do
      aid <- throwMaybeM errMsg $ Data.Map.Strict.lookup verbKey amap
      actionF <- getActionF aid
      case actionF of
        ImplicitStimulusF res -> do
          f <- throwLeftM caseMismatch res
          f desc
  where

    caseMismatch :: Text
    caseMismatch = "Implicit stimulus verb does not match expected type"

    errMsg :: Text
    errMsg = "Implicit stimulus verb not found"
