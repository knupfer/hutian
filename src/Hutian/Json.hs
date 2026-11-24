-- | Strict JSON scalar decoding and UTF-8 rendering shared by the stream and
-- MCP protocols.
module Hutian.Json
  ( jsonInt
  , jsonText
  ) where

import Data.Aeson (Result (..), Value (..), encode, fromJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
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

-- | Encode a JSON value as text for the terminal. 'encode' produces UTF-8
-- bytes, so they must be decoded as UTF-8 rather than unpacked byte by byte;
-- the latter turns every multibyte character into mojibake.
jsonText :: Value -> String
jsonText =
  Text.unpack
    . TextEncoding.decodeUtf8With TextEncodingError.lenientDecode
    . BL.toStrict
    . encode
