module TopLevel where
import           Control.Monad.Identity    (Identity (Identity))
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.State.Class (gets)
import           Data.Text                 (Text, pack)
import           GameState                 (clearNarration, modifyNarration)
import           Grammar.Parser            (parseTokens)
import           Grammar.Parser.Lexer      (Lexeme, lexify, tokens)
import           Model.GameState           (DisplayT, GameComputation,
                                            GameState (_evaluation, _narration),
                                            GameT, _actionConsequence,
                                            _playerAction, liftDisplay,
                                            transformToIO,
                                            updateActionConsequence)
import           Model.Parser              (Sentence)
import           Relude.String.Conversion  (ToText (toText))
import           System.Console.Haskeline  (InputT, defaultSettings,
                                            getInputLine, runInputT)

initComp :: GameComputation Identity ()
initComp = do
  pure ()

topLevel :: GameT IO ()
topLevel = runGame initComp
  where
    runGame :: GameComputation Identity () -> GameT IO ()
    runGame comp' = do
      transformToIO comp'
      liftDisplay displayResult
      transformToIO clearNarration
      attSentence <- trySentence <$> liftIO getInput
      case attSentence of
        Left err       -> runGame $ errorHandler err
        Right sentence -> runGame $ toGameComputation sentence

toGameComputation :: Sentence -> GameComputation Identity ()
toGameComputation sentence = do
  evaluator <- gets _evaluation
  evaluator sentence

displayResult :: DisplayT IO ()
displayResult = do
  narration <- gets _narration
  liftIO $ mapM_ print (_playerAction narration)
  liftIO $ mapM_ print (_actionConsequence narration)

trySentence :: Text -> Either Text Sentence
trySentence input = do
  case lexify tokens input of
    Left err      -> Left ("Lexeme fubar " <> err)
    Right lexemes -> case trySentence' lexemes of
      Left err       -> Left ("Parser fubar " <> err)
      Right sentence -> Right sentence
  where
    trySentence' :: [Lexeme] -> Either Text Sentence
    trySentence' lexemes = case parseTokens lexemes of
      Left err       -> Left $ toText err
      Right sentence -> Right sentence

getInput :: IO Text
getInput = do
  runInputT defaultSettings go
  where
    go :: InputT IO Text
    go = do
      minput <- getInputLine "What Do? "
      case minput of
        Nothing -> error "fail"
        Just input
          | null input -> go
          | otherwise  -> pure $ pack input

errorHandler :: Text -> GameComputation Identity ()
errorHandler err =
   modifyNarration $ updateActionConsequence ("lexer err " <>  err)
