-- | Strict JSON scalar decoding shared by the stream and MCP protocols.
module Hutian.Json
  ( jsonInt
  ) where

import Data.Aeson (Result (..), Value (..), fromJSON)
import qualified Data.Text as Text
import Text.Read (readMaybe)

-- | Decode an integer without accepting fractions or out-of-range numbers.
-- Aeson's 'Int' decoder uses 'toBoundedInteger', so @1.5@ and @1e100@ are
-- rejected; numeric-looking strings are accepted for servers that send string
-- ids.
jsonInt :: Value -> Maybe Int
jsonInt (String text) = readMaybe (Text.unpack text)
jsonInt value =
  case (fromJSON value :: Result Int) of
    Success result -> Just result
    Error _ -> Nothing
