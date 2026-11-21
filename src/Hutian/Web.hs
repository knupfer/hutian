{-# LANGUAGE OverloadedStrings #-}

-- | HTTP helpers for the model's web tools. Fetching is IO; request URL
-- construction and result formatting are pure so they can be tested without a
-- network.
module Hutian.Web
  ( FetchResult (..)
  , fetchResultText
  , fetchUrl
  , websearchRequestUrl
  ) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , fromException
  , tryJust
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import Hutian.Http.Body (readBodyAtMost)
import Hutian.Render (trailingNewline)
import Network.HTTP.Client
  ( Request (..)
  , Response (..)
  , brRead
  , newManager
  , parseRequest
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

-- | The useful parts of a fetched response.
data FetchResult = FetchResult
  { fetchStatus :: Int
  , fetchContentType :: String
  , fetchBody :: String
  }
  deriving stock (Eq, Show)

-- | Format a response as the text returned to the model. The status line makes
-- a non-success response explicit instead of looking like page content.
fetchResultText :: FetchResult -> String
fetchResultText result =
  trailingNewline $
    "HTTP "
      ++ show (fetchStatus result)
      ++ contentTypeSuffix
      ++ "\n\n"
      ++ fetchBody result
  where
    contentTypeSuffix
      | null (fetchContentType result) = ""
      | otherwise = " (" ++ fetchContentType result ++ ")"

-- | Fetch an @http@ or @https@ URL with a bounded response body.
fetchUrl :: String -> IO (Either String FetchResult)
fetchUrl url = do
  manager <- newManager tlsManagerSettings
  tryJust synchronous (fetch manager)
  where
    fetch manager = do
      initialRequest <- parseRequest url
      let request =
            initialRequest
              { method = "GET"
              , requestHeaders =
                  [ ("User-Agent", "hutian/0.8")
                  , ("Accept", "*/*")
                  ]
              , responseTimeout = responseTimeoutMicro webTimeoutMicros
              , redirectCount = 5
              , checkResponse = \_ _ -> pure ()
              }
      withResponse request manager $ \response -> do
        body <- readBodyAtMost maxWebBody (brRead (responseBody response))
        pure
          FetchResult
            { fetchStatus = statusCode (responseStatus response)
            , fetchContentType = maybe "" decodeHeader (lookup hContentType (responseHeaders response))
            , fetchBody = decodeBody body
            }

    synchronous :: SomeException -> Maybe String
    synchronous exception =
      case fromException exception :: Maybe SomeAsyncException of
        Just _ -> Nothing
        Nothing -> Just (displayException exception)

-- | Search endpoints should answer quickly; a stuck fetch must not hold an LLM
-- turn open indefinitely.
webTimeoutMicros :: Int
webTimeoutMicros = 30 * 1000 * 1000

-- | Bound what one web tool call can add to the conversation. The model gets a
-- useful prefix instead of an arbitrarily large page.
maxWebBody :: Int
maxWebBody = 256 * 1024

decodeBody :: BS.ByteString -> String
decodeBody =
  Text.unpack
    . TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

decodeHeader :: BS.ByteString -> String
decodeHeader = BS8.unpack

-- | Expand the configured search endpoint. @{query}@ and @%s@ are replaced by
-- the URL-encoded query. Without a placeholder the query is appended using
-- @q=@, which works for the common search-engine result pages.
websearchRequestUrl :: String -> String -> String
websearchRequestUrl template query
  | null template = template
  | "{query}" `isInfixOf` template =
      replaceAll "{query}" encoded template
  | "%s" `isInfixOf` template = replaceAll "%s" encoded template
  | "=" `isSuffixOf` template = template ++ encoded
  | "?" `isSuffixOf` template || "&" `isSuffixOf` template =
      template ++ "q=" ++ encoded
  | '?' `elem` template = template ++ "&q=" ++ encoded
  | otherwise = template ++ "?q=" ++ encoded
  where
    encoded = BS8.unpack (urlEncode True (BS8.pack query))

replaceAll :: String -> String -> String -> String
replaceAll needle replacement = go
  where
    go [] = []
    go text
      | needle `isInfixOf` text =
          case breakOn needle text of
            (before, after) -> before ++ replacement ++ go after
      | otherwise = text

breakOn :: String -> String -> (String, String)
breakOn needle = search
  where
    search [] = ([], [])
    search text@(character : rest)
      | needle `isPrefixOf` text = ([], drop (length needle) text)
      | otherwise =
          let (before, after) = search rest
           in (character : before, after)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (left : lefts) (right : rights) =
  left == right && isPrefixOf lefts rights
