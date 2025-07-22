module Actions.Percieve.Look (agentCanSee,agentCannotSee, manageImplicitStimulusProcess) where

import           Control.Monad.Identity     (Identity)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader.Class (asks)
import           Data.Text                  (Text)
import           Error                      (throwMaybeM)
import           GameState                  (modifyNarration)
import           Location                   (getLocationActionMapM,
                                             getPlayerLocationM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             Config (_actionMap),
                                             Location (_title),
                                             ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb),
                                             updateActionConsequence)
import           Model.Mappings             (GIDToDataMap (_getGIDToDataMap))
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
      where
        errMsg = "Programmer Error: No implicit stimulus action found for verb: " <> toText isv
        verbKey = ImplicitStimulusKey isv
