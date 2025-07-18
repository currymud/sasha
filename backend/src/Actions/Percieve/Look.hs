module Actions.Percieve.Look (agentCanSee,agentCannotSee, manageImplicitStimulusProcess) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.RWS          (MonadReader (local))
import qualified Data.Map.Strict
import           Data.Text                  (Text)
import           Error                      (throwMaybeM)
import           Location                   (getLocationActionMapM,
                                             getLocationM, getPlayerLocationM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             Config (_actionMap),
                                             GameComputation, GameStateExceptT,
                                             Location (_title),
                                             ProcessImplicitStimulusVerb (ProcessImplicitStimulusVerb),
                                             ResolutionT (ResolutionT))
import           Model.Mappings             (GIDToDataMap (_getGIDToDataMap))
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)
import           Model.Parser.GCase         (VerbKey (ImplicitStimulusKey))
import           Relude.String.Conversion   (ToText (toText))

agentCanSee :: ActionF
agentCanSee = ImplicitStimulusAction (\loc ->
  pure $ ResolutionT $ liftIO $ print ("You see: " <> _title loc))

agentCannotSee :: Text -> ActionF
agentCannotSee nosee = ImplicitStimulusAction $ \loc ->
  pure $ ResolutionT $ liftIO $ print nosee

manageImplicitStimulusProcess :: ProcessImplicitStimulusVerb
manageImplicitStimulusProcess = ProcessImplicitStimulusVerb (\isv ->
  pure $ ResolutionT $ do
    actionMap <- asks (_getGIDToDataMap . _actionMap)
    locationActionMap <- getLocationActionMapM
    let errMsg = "Programmer Error: No implicit stimulus action found for verb: " <> toText isv
        verbKey = ImplicitStimulusKey isv
    aid <- throwMaybeM errMsg $ Data.Map.Strict.lookup verbKey locationActionMap
    throwMaybeM errMsg $ Data.Map.Strict.lookup aid actionMap
    )


noSeeLoc :: Text -> GameComputation
noSeeLoc nosee = pure $ ResolutionT $ liftIO $ print nosee
