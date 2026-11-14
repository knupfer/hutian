-- | Pure command-line option parsing shared by the shell and the relay.
--
-- The parser is deliberately strict: unknown options, missing values,
-- duplicated options, and bare positional arguments are rejected. Callers add
-- their own usage text to the returned error.
module Hutian.Cli
  ( OptionKind (..)
  , Option
  , optionName
  , optionValue
  , parseOptions
  , hasOption
  , optionValueFor
  , flagPresent
  , missingOptions
  ) where

import Data.List (find)
import Data.Maybe (isJust)

-- | Whether an option takes a value or is a bare flag.
data OptionKind
  = OptionValue
  | OptionFlag
  deriving stock (Eq, Show)

-- | One parsed option. A flag has no value.
data Option = Option
  { optionName :: String
  , optionValue :: Maybe String
  }
  deriving stock (Eq, Show)

-- | Parse arguments against a list of known options.
--
-- A value option consumes the following argument even when it looks like a
-- flag, matching the usual @--name value@ convention. Repeating any option is
-- an error so configuration cannot silently depend on argument order.
parseOptions :: [(String, OptionKind)] -> [String] -> Either String [Option]
parseOptions spec = go []
  where
    go parsed [] = Right (reverse parsed)
    go parsed (argument : rest)
      | Just kind <- lookup argument spec =
          if any ((== argument) . optionName) parsed
            then Left ("duplicate argument: " ++ argument)
            else case kind of
              OptionFlag -> go (Option argument Nothing : parsed) rest
              OptionValue ->
                case rest of
                  [] -> Left ("missing value for " ++ argument)
                  value : more -> go (Option argument (Just value) : parsed) more
      | otherwise = Left ("unknown argument: " ++ argument)

hasOption :: String -> [Option] -> Bool
hasOption name = isJust . find ((== name) . optionName)

-- | The value of an option that takes one.
optionValueFor :: String -> [Option] -> Maybe String
optionValueFor name options =
  optionValue =<< find ((== name) . optionName) options

-- | Whether a bare flag was present.
flagPresent :: String -> [Option] -> Bool
flagPresent = hasOption

-- | Required option names that were not supplied, in the order given.
missingOptions :: [String] -> [Option] -> [String]
missingOptions required options =
  filter (\name -> not (hasOption name options)) required
