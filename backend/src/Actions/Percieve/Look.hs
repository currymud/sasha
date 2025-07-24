module Actions.Percieve.Look (agentCanSee,agentCannotSee, manageImplicitStimulusProcess) where

import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Reader.Class (asks)
import qualified Data.Map.Strict
import           Data.Text                  (Text)
import           Error                      (throwMaybeM)
import           GameState                  (modifyNarration)
import           Location                   (getLocationActionMapM,
                                             getPlayerLocationM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             Config (_actionMap),
                                             GameComputation, Location (_title),
                                             ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb),
                                             updateActionConsequence)
import           Model.Mappings             (GIDToDataMap (_getGIDToDataMap))
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Model.Parser.GCase         (VerbKey (ImplicitStimulusKey))
import           Relude.String.Conversion   (ToText (toText))

agentCanSee :: ActionF
agentCanSee = ImplicitStimulusAction (\loc ->
   modifyNarration $ updateActionConsequence ("You see: " <> _title loc))

agentCannotSee :: Text -> ActionF
agentCannotSee nosee = ImplicitStimulusAction $ \_ ->
  modifyNarration $ updateActionConsequence nosee

manageImplicitStimulusProcess :: ProcessImplicitStimulusVerb
manageImplicitStimulusProcess = ProcessImplicitStimulusVerb go
  where
    go :: ImplicitStimulusVerb -> GameComputation Identity ()
    go isv = do
      actionMap <- asks (_getGIDToDataMap . _actionMap)
      locationActionMap <- getLocationActionMapM
      aid <- throwMaybeM errMsg $ Data.Map.Strict.lookup verbKey locationActionMap
      actionF <- throwMaybeM errMsg $ Data.Map.Strict.lookup aid actionMap
      case actionF of
        ImplicitStimulusAction actionFunc -> do
          currentLocation <- getPlayerLocationM
          actionFunc currentLocation
        _ -> throwError errMsg'
      where
        errMsg' = "Programmer Error: Somehow tried to process non-implicit stimulus action for: " <> toText isv
        errMsg = "Programmer Error: No implicit stimulus action found for verb: " <> toText isv
        verbKey = ImplicitStimulusKey isv

