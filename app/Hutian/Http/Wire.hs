-- | Small shared socket helpers. They cover the byte-oriented framing used by
-- the relay, the sandbox-side relay client, and the HTTP test servers.
module Hutian.Http.Wire
  ( WireError (..)
  , recvUntil
  , recvExactly
  , recvAll
  , recvAllLimited
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)

data WireError
  = WireTooLarge
  | WireClosed
  deriving stock (Eq, Show)

-- | Read until the marker appears, returning the marker-terminated head and
-- any bytes that followed it.
recvUntil :: Socket -> ByteString -> Int -> IO (Either WireError (ByteString, ByteString))
recvUntil connection marker limit = go BS.empty
  where
    go accumulated
      | Just (before, after) <- splitAtMarker marker accumulated
      , BS.length before <= limit =
          pure (Right (before <> marker, after))
      | BS.length accumulated > limit = pure (Left WireTooLarge)
      | otherwise = do
          chunk <- recv connection 8192
          if BS.null chunk
            then pure (Left WireClosed)
            else go (accumulated <> chunk)

-- | Read exactly the requested number of bytes, reusing any buffered input.
recvExactly :: Socket -> Int -> ByteString -> IO (Either WireError ByteString)
recvExactly connection expected leftover
  | BS.length leftover >= expected = pure (Right (BS.take expected leftover))
  | otherwise = go leftover (expected - BS.length leftover)
  where
    go accumulated remaining
      | remaining <= 0 = pure (Right accumulated)
      | otherwise = do
          chunk <- recv connection (min 65536 remaining)
          if BS.null chunk
            then pure (Left WireClosed)
            else go (accumulated <> chunk) (remaining - BS.length chunk)

-- | Read until the peer closes the connection.
recvAll :: Socket -> IO ByteString
recvAll connection = go []
  where
    go chunks = do
      chunk <- recv connection 32768
      if BS.null chunk
        then pure (BS.concat (reverse chunks))
        else go (chunk : chunks)

-- | Read until the peer closes the connection, failing instead of buffering
-- more than the given limit.
recvAllLimited :: Socket -> Int -> IO (Either WireError ByteString)
recvAllLimited connection limit = go [] 0
  where
    go chunks total = do
      chunk <- recv connection 32768
      if BS.null chunk
        then pure (Right (BS.concat (reverse chunks)))
        else
          let total' = total + BS.length chunk
           in if total' > limit
                then pure (Left WireTooLarge)
                else go (chunk : chunks) total'

splitAtMarker :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
splitAtMarker marker bytes =
  case BS.breakSubstring marker bytes of
    (before, after)
      | BS.null after -> Nothing
      | otherwise -> Just (before, BS.drop (BS.length marker) after)
