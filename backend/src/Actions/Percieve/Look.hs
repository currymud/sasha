module Actions.Percieve.Look (agentCanSee,agentCannotSee) where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text)
import           Model.GameState        (ActionF (ComputeAction),
                                         ProcessSentence (ImplicitStimulusF),
                                         ResolutionT (ResolutionT))

agentCanSee :: ActionF
agentCanSee = ComputeAction $ ImplicitStimulusF $ Right $ \msg ->
  ResolutionT  $ liftIO $ print msg

agentCannotSee :: Text -> ActionF
agentCannotSee nosee = ComputeAction $ ImplicitStimulusF $ Left $ ResolutionT $
  liftIO $ print nosee
