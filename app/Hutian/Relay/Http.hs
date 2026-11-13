{-# LANGUAGE OverloadedStrings #-}

-- | Pure HTTP wire helpers for the host-side LLM relay. The relay only speaks
-- a deliberately small subset of HTTP: one POST per connection with a
-- Content-Length body. Keeping the parsing here makes the relay testable
-- without sockets or gVisor.
module Hutian.Relay.Http
  ( RequestHead (..)
  , parseRequestHead
  , headerValue
  , trimKey
  , redactKey
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError

-- | The parsed request line and headers, with header names lowercased.
data RequestHead = RequestHead
  { headMethod :: BS.ByteString
  , headTarget :: BS.ByteString
  , headVersion :: BS.ByteString
  , headHeaders :: [(BS.ByteString, BS.ByteString)]
  }
  deriving stock (Eq, Show)

-- | Parse a complete request head, including the trailing CRLFCRLF.
parseRequestHead :: BS.ByteString -> Either String RequestHead
parseRequestHead raw =
  case splitOnce "\r\n\r\n" raw of
    Nothing -> Left "incomplete request head"
    Just (headPart, _) ->
      case map stripCarriageReturn (BS8.split '\n' headPart) of
        [] -> Left "empty request"
        (requestLine : headerLines) ->
          case BS8.words requestLine of
            [method, target, version] -> do
              headers <- traverse parseHeader headerLines
              pure
                RequestHead
                  { headMethod = method
                  , headTarget = target
                  , headVersion = version
                  , headHeaders = headers
                  }
            _ -> Left "malformed request line"

parseHeader :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
parseHeader line =
  case BS8.break (== ':') line of
    (name, rest)
      | not (BS.null name) && not (BS.null rest) ->
          Right
            ( BS8.map toLower name
            , BS8.dropWhile (== ' ') (BS8.drop 1 rest)
            )
    _ -> Left "malformed header"

-- | Look up a header by case-insensitive name.
headerValue :: BS.ByteString -> RequestHead -> Maybe BS.ByteString
headerValue name request =
  lookup (BS8.map toLower name) (headHeaders request)

-- | Remove surrounding ASCII whitespace from a key read from a file or stdin.
trimKey :: BS.ByteString -> BS.ByteString
trimKey = BS.dropWhile isKeySpace . BS.dropWhileEnd isKeySpace
  where
    isKeySpace byte = byte == 32 || (byte >= 9 && byte <= 13)

-- | Remove a secret from a log line so relay errors never echo the key.
redactKey :: BS.ByteString -> String -> String
redactKey key message
  | BS.null key = message
  | otherwise =
      Text.unpack $
        Text.replace
          (TextEncoding.decodeUtf8With TextEncodingError.lenientDecode key)
          "<redacted>"
          (Text.pack message)

splitOnce :: BS.ByteString -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitOnce needle haystack =
  case BS.breakSubstring needle haystack of
    (before, after)
      | BS.null after -> Nothing
      | otherwise -> Just (before, BS.drop (BS.length needle) after)

stripCarriageReturn :: BS.ByteString -> BS.ByteString
stripCarriageReturn = BS.dropWhileEnd (== 13)
