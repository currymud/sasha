module Actions.Percieve.Look (locationSeen,agentCannotSee, manageLookProcess) where

import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Text                  (Text)
import           GameState                  (getPlayerActionF)
import           Location                   (getLocationM, getPlayerLocationM)
import           Model.GameState            (ActionF (ImplicitStimulusAction),
                                             GameComputation, GameStateExceptT,
                                             Location (_title),
                                             ProcessSentence (ImplicitStimulusF),
                                             ResolutionT (ResolutionT))

import           Control.Monad.RWS          (MonadReader (local))
import           Model.Parser.Atomics.Verbs (ImplicitStimulusVerb)

-- has the ability to see and will compute a Location
locationSeen :: ActionF
locationSeen = ImplicitStimulusAction (\loc -> pure $ ResolutionT $ pure $ do
  liftIO $ print ("You see: " <> _title loc))

agentCannotSee :: Text -> ActionF
agentCannotSee nosee = ImplicitStimulusAction $ pure $ ResolutionT $ pure $ do
  liftIO $ print nosee

manageImplicitStimulusProcess :: ProcessSentence
manageImplicitStimulusProcess = ImplicitStimulusF (\isv -> do
  pure $ ResolutionT ( pure () :: GameStateExceptT ()))
  {-
  pure $ ResolutionT $ pure $ do
     amap <- _locationActionManagement <$> getPlayerLocationM
     case Data.Map.Strict.lookup verbKey amap of
       Nothing -> liftIO $ print "can't do that here"
       Just res -> case res of
                     Left res' -> res'

-}
noSeeLoc :: Text -> GameComputation
noSeeLoc nosee = pure $ ResolutionT $ do
  pure $ liftIO $ print nosee

