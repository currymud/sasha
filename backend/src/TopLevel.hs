module TopLevel where
import           Control.Monad.Identity    (Identity)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.State       (get, modify')
import           Control.Monad.State.Class (gets, put)
import           Data.Kind                 (Type)
import qualified Data.Map.Strict
import           Data.Text                 (Text, empty, pack, unpack)
import           Debug.Trace               (trace)
import           GameState                 (clearNarration, modifyNarration)
import           Grammar.Parser            (parseTokens)
import           Grammar.Parser.Lexer      (Lexeme, lexify, tokens)
import           Model.GameState           (DisplayT, GameComputation,
                                            GameState (_evaluation, _narration, _systemEffectRegistry, _world),
                                            GameT, Object (..),
                                            SystemEffect (PerceptionSystemEffect),
                                            SystemEffectConfig (SystemEffectConfig, _systemEffect, _systemEffectManagement),
                                            World (..), _actionConsequence,
                                            _playerAction, liftDisplay,
                                            transformToIO,
                                            updateActionConsequence)
import           Model.GameState.Mappings
import           Model.Parser              (Sentence)
import           Relude.String.Conversion  (ToText (toText))
import           System.Console.Haskeline  (InputT, defaultSettings,
                                            getInputLine, runInputT)

debugObjectMap :: GameComputation Identity ()
debugObjectMap = do
  world <- gets _world
  let objectMap = _getGIDToDataMap $ _objectMap world
      objectEntries = Data.Map.Strict.toList objectMap
  trace ("=== OBJECT MAP DEBUG ===") $ pure ()
  mapM_ (\(gid, obj) -> trace ("Object GID " ++ show gid ++ ": " ++ (Data.Text.unpack $ _shortName obj)) $ pure ()) objectEntries
  trace ("=== END OBJECT MAP ===") $ pure ()

batchProcess :: [Text] -> GameComputation Identity ()
batchProcess inputs = do
  case mapM trySentence inputs of
    Left err        -> errorHandler err
    Right sentences -> mapM_ toGameComputation sentences

testOpenEyesGetRobe :: GameT IO ()
testOpenEyesGetRobe = transformToIO (batchProcess ["open eyes", "get robe"])

initComp :: GameComputation Identity ()
initComp = pure ()

topLevel :: GameT IO ()
topLevel = runGame initComp

runGame :: GameComputation Identity () -> GameT IO ()
runGame comp' = do
  transformToIO debugObjectMap
  transformToIO comp'
  liftDisplay displayResult
  transformToIO clearNarration
  attSentence <- trySentence <$> liftIO getInput
  case attSentence of
    Left err       -> runGame $ errorHandler err
    Right sentence -> runGame $ processWithSystemEffects sentence

processWithSystemEffects :: Sentence -> GameComputation Identity ()
processWithSystemEffects sentence = do
  toGameComputation sentence
  -- ToDo System Effects processing
  --
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
trySentence input = case lexify tokens input of
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
getInput = runInputT defaultSettings go
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
