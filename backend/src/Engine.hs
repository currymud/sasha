module Engine where
import           Control.Monad.State (get)
import qualified Data.Map.Strict
import           Data.Text           (Text)
import           Error               (throwMaybeM)
import           Model.GameState     (GameState (_player, _world),
                                      GameStateExceptT, Location,
                                      Object (_objectActionManagement),
                                      ResolutionT (ResolutionT),
                                      World (_objectMap))
import           Model.Mappings      (GIDToDataMap (_getGIDToDataMap))
import           Model.Parser        (Sentence)
import           Model.Parser.GCase  (VerbKey, mkVerbKey)


engine :: Sentence -> GameStateExceptT (ResolutionT ())
engine sentence = do

  pure placeholder
  where
    verbKey = mkVerbKey sentence

  {-
Can the player perform the action?
If yes, return a ResolutionT that computes Location
If no, return a ResolutionT that computes error message
     -}
       {-
playerAble :: VerbKey
                -> GameStateExceptT (Either (ResolutionT ()) (ResolutionT Location))
playerAble verbKey = do
 obj <- _objectActionManagement <$> getPlayerObject

 pure $ Left $ ResolutionT $ pure ()

getPlayerObject :: GameStateExceptT Object
getPlayerObject = do
  gs <- get
  let objectMap = (_getGIDToDataMap . _objectMap . _world) gs
      oid = (_object . _player) gs
  throwMaybeM notFound $ Data.Map.Strict.lookup oid objectMap
  where
    notFound :: Text
    notFound = "Player object not found in the object map."
-}
placeholder :: ResolutionT ()
placeholder = do
  ResolutionT $ pure ()
