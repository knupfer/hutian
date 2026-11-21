-- | Bounded reads from a streaming HTTP body. Shared by the web and MCP
-- clients so one tool call cannot add an unbounded response.
module Hutian.Http.Body
  ( readBodyAtMost
  ) where

import qualified Data.ByteString as BS

readBodyAtMost :: Int -> IO BS.ByteString -> IO BS.ByteString
readBodyAtMost limit reader = go 0 []
  where
    go total collected
      | total >= limit = pure (BS.concat (reverse collected))
      | otherwise = do
          chunk <- reader
          if BS.null chunk
            then pure (BS.concat (reverse collected))
            else do
              let kept = BS.take (limit - total) chunk
              go (total + BS.length kept) (kept : collected)
