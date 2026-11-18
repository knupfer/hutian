{-# LANGUAGE OverloadedStrings #-}

-- | The private framing between the sandboxed shell and the host relay.
--
-- A request is just the request body, ended by the client closing its write
-- half. A response starts with a four-byte big-endian status code and then the
-- body until the relay closes the connection. The relay never interprets
-- client-controlled request text: there is no request line, no headers, and no
-- target to validate.
module Hutian.Relay.Protocol
  ( statusFrameLength
  , encodeStatus
  , decodeStatus
  ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
-- Imported for its LiquidHaskell specification, which defines `isLeft`.
import Data.Either ()
import Data.Word (Word8)

-- | Every response starts with exactly this many bytes.
{-@ statusFrameLength :: {v:Int | v == 4} @-}
statusFrameLength :: Int
statusFrameLength = 4

-- | Encode an HTTP status code as four big-endian bytes.
{-@ encodeStatus :: code:Int -> {v:ByteString | bslen v == 4} @-}
encodeStatus :: Int -> ByteString
encodeStatus code =
  BS.pack
    [ byteAt 24
    , byteAt 16
    , byteAt 8
    , byteAt 0
    ]
  where
    byteAt shift = fromIntegral (code `shiftR` shift) :: Word8

-- | Decode and validate a four-byte status frame. Statuses outside the HTTP
-- range are rejected instead of being passed through as a number.
{-@ decodeStatus :: frame:ByteString ->
      {v:Either String Int | not (isLeft v) => bslen frame == 4} @-}
decodeStatus :: ByteString -> Either String Int
decodeStatus frame
  | BS.length frame /= statusFrameLength = Left "relay: invalid status frame"
  | code < 100 || code > 599 = Left "relay: invalid status code"
  | otherwise = Right code
  where
    code =
      BS.foldl' (\accumulator byte -> accumulator * 256 + fromIntegral byte) 0 frame
