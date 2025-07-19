module TopLevel where
import           Control.Monad.State      (MonadIO (liftIO), MonadState (get),
                                           gets)
import           Data.Text                (Text, pack)
import           Grammar.Parser           (parseTokens)
import           Grammar.Parser.Lexer     (Lexeme, lexify, tokens)
import           Model.GameState          (GameComputation,
                                           GameState (_narration),
                                           GameStateExceptT,
                                           ResolutionT (ResolutionT, runResolutionT),
                                           _actionConsequence, _playerAction)
import           Model.Parser             (Sentence)
import           Relude.String.Conversion (ToText (toText))
import           System.Console.Haskeline (InputT, defaultSettings,
                                           getInputLine, runInputT)

initComp :: GameComputation
initComp = do
  pure $ ResolutionT $ pure ()

topLevel :: GameStateExceptT ()
topLevel = runGame initComp
  where
    runGame :: GameComputation -> GameStateExceptT ()
    runGame comp' = do
      comp :: ResolutionT () <- comp'
      runResolutionT comp
      displayResult

displayResult :: GameStateExceptT ()
displayResult = do
  narration <- gets _narration
  liftIO $ mapM_ print (_playerAction narration)
  liftIO $ mapM_ print (_actionConsequence narration)
  pure ()

inputAction :: GameStateExceptT (ResolutionT ())
inputAction = do
  input <- liftIO getInput
  case lexify tokens input of
    Left err      -> errorHandler err
    Right lexemes -> case trySentence lexemes of
      Left err       -> errorHandler err
      Right sentence -> do
        placeHolder

getInput :: IO Text
getInput = do
  runInputT defaultSettings go
  where
    go :: InputT IO Text
    go = do
      minput <- getInputLine "What Do? "
      case minput of
        Nothing    -> go
        Just input -> pure $ pack input

errorHandler :: Text -> GameStateExceptT (ResolutionT ())
errorHandler err = do
  pure $ ResolutionT $ do
    liftIO $ print $ "Lexer failed: " <> err

trySentence :: [Lexeme] -> Either Text Sentence
trySentence lexemes = case parseTokens lexemes of
  Left err       -> Left $ toText err
  Right sentence -> Right sentence

placeHolder :: GameStateExceptT (ResolutionT ())
placeHolder = pure $ ResolutionT $ pure ()
