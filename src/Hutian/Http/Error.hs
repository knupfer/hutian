-- | Synchronous failure handling shared by the HTTP clients. An async
-- exception, including the one used to cancel a request worker, must keep
-- propagating, so only synchronous failures become error messages here.
module Hutian.Http.Error
  ( trySync
  ) where

import Control.Exception
  ( SomeAsyncException
  , displayException
  , fromException
  , tryJust
  )

-- | Run an action, returning the message of a synchronous exception. Async
-- exceptions are rethrown.
trySync :: IO a -> IO (Either String a)
trySync = tryJust synchronous
  where
    synchronous exception =
      case fromException exception :: Maybe SomeAsyncException of
        Just _ -> Nothing
        Nothing -> Just (displayException exception)
