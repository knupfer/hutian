{-# LANGUAGE OverloadedStrings #-}

-- | A concise, human-readable summary of the tool definitions sent to the
-- model. High verbosity uses it instead of printing the full JSON schemas.
module Hutian.Tool.Summary
  ( toolSummaryLines
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | One entry per tool: a @# name(params)@ line followed by the complete
-- description on its own lines. Each description line is trimmed and has its
-- internal whitespace collapsed, but line breaks are preserved.
toolSummaryLines :: [Value] -> [String]
toolSummaryLines = concatMap summaryLines

summaryLines :: Value -> [String]
summaryLines definition =
  case objectField "function" definition of
    Just function ->
      case stringField "name" function of
        Just name ->
          let parameters = maybe [] parameterList (objectField "parameters" function)
              signature = name ++ "(" ++ intercalate ", " parameters ++ ")"
              description = fromMaybe "" (stringField "description" function)
           in ("# " ++ signature) : descriptionLines description
        Nothing -> []
    Nothing -> []

objectField :: Text.Text -> Value -> Maybe Value
objectField key value =
  case value of
    Object fields -> KeyMap.lookup (AesonKey.fromText key) fields
    _ -> Nothing

stringField :: Text.Text -> Value -> Maybe String
stringField key value =
  case objectField key value of
    Just (String text) -> Just (Text.unpack text)
    _ -> Nothing

parameterList :: Value -> [String]
parameterList parameters =
  map renderParameter (requiredParameters ++ optionalParameters)
  where
    properties =
      case objectField "properties" parameters of
        Just (Object fields) -> fields
        _ -> KeyMap.empty
    requiredNames =
      case objectField "required" parameters of
        Just (Array values) ->
          [Text.unpack name | String name <- toList values]
        _ -> []
    requiredParameters =
      [ (name, schema, True)
      | name <- requiredNames
      , Just schema <- [lookupProperty name properties]
      ]
    optionalNames =
      sort
        [ AesonKey.toText key
        | key <- KeyMap.keys properties
        , AesonKey.toText key `notElem` map Text.pack requiredNames
        ]
    optionalParameters =
      [ (Text.unpack name, schema, False)
      | name <- optionalNames
      , Just schema <- [KeyMap.lookup (AesonKey.fromText name) properties]
      ]

lookupProperty :: String -> KeyMap.KeyMap Value -> Maybe Value
lookupProperty name = KeyMap.lookup (AesonKey.fromString name)

renderParameter :: (String, Value, Bool) -> String
renderParameter (name, schema, required) =
  name
    ++ (if required then "" else "?")
    ++ ": "
    ++ renderType schema

renderType :: Value -> String
renderType schema =
  case objectField "enum" schema of
    Just (Array values)
      | not (Vector.null values) -> intercalate "|" (map enumValue (toList values))
    _ ->
      case objectField "type" schema of
        Just (String kind) -> abbreviate (Text.unpack kind)
        _ -> "any"

enumValue :: Value -> String
enumValue value =
  case value of
    String text -> Text.unpack text
    _ -> "any"

abbreviate :: String -> String
abbreviate kind =
  case kind of
    "string" -> "str"
    "integer" -> "int"
    "number" -> "num"
    "boolean" -> "bool"
    other -> other

descriptionLines :: String -> [String]
descriptionLines text =
  [unwords wordsInLine | wordsInLine <- map words (lines text), not (null wordsInLine)]
