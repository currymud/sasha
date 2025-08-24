module GameState.FieldValidation where
import           Control.Monad.Error.Class (MonadError (throwError))
import           Data.Text                 (Text)
import qualified Data.Text

-- Validate short name field (10 character limit like existing WithShortName)
validateShortName :: (MonadError Text m) => Text -> m Text
validateShortName text
  | Data.Text.null text = throwError "Short name cannot be empty"
  | Data.Text.length text > 10 = throwError "Short name cannot exceed 10 characters"
  | otherwise = pure $ Data.Text.strip text

-- Validate description field (reasonable length limit)
validateDescription :: (MonadError Text m) => Text -> m Text
validateDescription text
  | Data.Text.null text = throwError "Description cannot be empty"
  | Data.Text.length text > 200 = throwError "Description cannot exceed 200 characters"
  | otherwise = pure $ Data.Text.strip text

-- Validate title field (reasonable length limit)
validateTitle :: (MonadError Text m) => Text -> m Text
validateTitle text
  | Data.Text.null text = throwError "Title cannot be empty"
  | Data.Text.length text > 50 = throwError "Title cannot exceed 50 characters"
  | otherwise = pure $ Data.Text.strip text
