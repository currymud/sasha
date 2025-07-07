module Error (throwMaybeM,throwLeftM,throwRightM) where

import           Control.Monad.Error.Class (MonadError (throwError))
import           Data.Text                 (Text)

throwMaybeM :: (MonadError Text m) => Text -> Maybe a -> m a
throwMaybeM _ (Just a)     = pure a
throwMaybeM errMsg Nothing = throwError errMsg

throwLeftM :: (MonadError Text m) => Text -> Either a b -> m b
throwLeftM _ (Right b)     = pure b
throwLeftM errMsg (Left _) = throwError errMsg

throwRightM :: (MonadError Text m) => Text -> Either a b -> m a
throwRightM _ (Left a)       = pure a
throwRightM errMsg (Right _) = throwError errMsg
