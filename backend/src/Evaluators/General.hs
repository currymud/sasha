module Evaluators.General where
import qualified Data.Map.Strict
import           Data.Text                     (Text)
import           Error                         (throwMaybeM)
import           Location                      (getLocation)
import           Model.GameState               (ActionF (ImplicitStimulusF),
                                                Location (Location),
                                                ResolutionF (ResolutionF))
import           Model.GID                     (GID)
import           Model.Parser                  (Sentence (Imperative))
import           Model.Parser.Atomics.Verbs    (ImplicitStimulusVerb)
import           Model.Parser.Composites.Verbs (Imperative (StimulusVerbPhrase),
                                                StimulusVerbPhrase (ImplicitStimulusVerb))
import           Model.Parser.GCase            (VerbKey, mkVerbKey)
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
evalImplicitStimulusVerb :: ImplicitStimulusVerb
                              -> (VerbKey -> Location -> ResolutionF)
evalImplicitStimulusVerb isv = \verbkey (Location _ _ amap) -> ResolutionF $ do
  aid <- throwMaybeM errMsg $ Data.Map.Strict.lookup verbkey amap
  pure ()
  where
    errMsg :: Text
    errMsg = "Implicit stimulus verb " <> toText isv <> " not found"
