-- | Strict JSON scalar decoding, object field access, and UTF-8 rendering
-- shared by the protocol and rendering code.
module Hutian.Json
  ( jsonInt
  , jsonText
  , objectField
  , textField
  , stringField
  , lookupTextField
  , objectArray
  , lookupObjectArray
  , decodeUtf8Lenient
  , decodeUtf8LenientText
  ) where

import Data.Aeson (Result (..), Value (..), encode, fromJSON)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
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
jsonText = decodeUtf8Lenient . BL.toStrict . encode

-- | Look up a field of a JSON object.
objectField :: Text.Text -> Value -> Maybe Value
objectField key (Object fields) = KeyMap.lookup (AesonKey.fromText key) fields
objectField _ _ = Nothing

-- | A string field of a JSON object, decoded as text.
lookupTextField :: Text.Text -> KeyMap.KeyMap Value -> Maybe Text.Text
lookupTextField key fields =
  case KeyMap.lookup (AesonKey.fromText key) fields of
    Just (String text) -> Just text
    _ -> Nothing

-- | A string field of a JSON value, decoded as text.
textField :: Text.Text -> Value -> Maybe Text.Text
textField key (Object fields) = lookupTextField key fields
textField _ _ = Nothing

-- | A string field of a JSON object, decoded as a 'String'.
stringField :: Text.Text -> Value -> Maybe String
stringField key = fmap Text.unpack . textField key

-- | The object elements of an array field of a JSON object.
lookupObjectArray :: Text.Text -> KeyMap.KeyMap Value -> [KeyMap.KeyMap Value]
lookupObjectArray key fields =
  case KeyMap.lookup (AesonKey.fromText key) fields of
    Just (Array items) -> [item | Object item <- Vector.toList items]
    _ -> []

-- | The object elements of an array field of a JSON value.
objectArray :: Text.Text -> Value -> [KeyMap.KeyMap Value]
objectArray key (Object fields) = lookupObjectArray key fields
objectArray _ _ = []

-- | Decode bytes as UTF-8, replacing malformed sequences instead of failing.
decodeUtf8LenientText :: BS.ByteString -> Text.Text
decodeUtf8LenientText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

-- | Decode bytes as UTF-8 for display. 'decodeUtf8LenientText' keeps multi-byte
-- sequences intact; unpacking the bytes directly would produce mojibake.
decodeUtf8Lenient :: BS.ByteString -> String
decodeUtf8Lenient = Text.unpack . decodeUtf8LenientText
