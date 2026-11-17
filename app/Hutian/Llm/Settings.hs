-- | Launcher settings for the model connection. Parsing is pure so the shell
-- can reject bad arguments before any IO or request happens.
module Hutian.Llm.Settings
  ( LlmUrl (..)
  , ModelName (..)
  , SystemPrompt (..)
  , LlmSettings (..)
  , parseLlmSettings
  , parseLlmSettingsWithFiles
  , llmFilePaths
  , urlPath
  ) where

import Data.Char (isControl, isSpace)
import Data.List (isPrefixOf, nub)
import Data.Maybe (catMaybes)
import Hutian.Cli
  ( OptionKind (..)
  , hasOption
  , missingOptions
  , optionValueFor
  , parseOptions
  )
import Hutian.Policy (ModelShell (..), modelShellFromArgument)

-- | Connection settings supplied by the launcher.
data LlmSettings = LlmSettings
  { settingUrl :: LlmUrl
  , settingModel :: ModelName
  , settingSystemPrompt :: SystemPrompt
  , settingSocket :: Maybe FilePath
  -- | How the model's shell tool is handled. Term mode is unaffected.
  , settingModelShell :: ModelShell
  }
  deriving stock (Eq, Show)

newtype LlmUrl = LlmUrl {unLlmUrl :: String}
  deriving stock (Eq, Show)

newtype ModelName = ModelName {unModelName :: String}
  deriving stock (Eq, Show)

newtype SystemPrompt = SystemPrompt {unSystemPrompt :: String}
  deriving stock (Eq, Show)

-- | Parse the launcher-provided connection settings. All three connection
-- options are required so the shell never falls back to ambient environment
-- variables. '--llama-socket' is optional and moves the HTTP request to the
-- host relay. '--model-shell' selects deny, ask, or allow for the model's shell
-- tool; its absence denies the tool, not the user's term mode.
parseLlmSettings :: [String] -> Either String LlmSettings
parseLlmSettings = parseLlmSettingsWithFiles []

-- | Parse settings with already-loaded contents for the file-backed options.
-- 'Hutian.Llm.loadLlmSettings' reads and removes the files before calling this
-- function so the contents never appear in the process listing.
parseLlmSettingsWithFiles :: [(FilePath, String)] -> [String] -> Either String LlmSettings
parseLlmSettingsWithFiles loaded arguments = do
  options <- withUsage (parseOptions llmOptionSpec arguments)
  let missingOption inline file =
        [inline | not (hasOption inline options || hasOption file options)]
      missing =
        missingOptions ["--llama-url"] options
          ++ missingOption "--model" "--model-file"
          ++ missingOption "--system-prompt" "--system-prompt-file"
  case missing of
    [] -> do
      url <- required "--llama-url" options
      model <- valueOrFile "--model" "--model-file" loaded options
      systemPrompt <- valueOrFile "--system-prompt" "--system-prompt-file" loaded options
      policy <- policyOption options
      settingsFrom
        (LlmUrl url)
        (ModelName model)
        (SystemPrompt systemPrompt)
        (optionValueFor "--llama-socket" options)
        policy
    _ -> Left (missingMessage missing)
  where
    required name options =
      case optionValueFor name options of
        Just value -> Right value
        Nothing -> Left ("missing required argument: " ++ name ++ "\n" ++ llmUsage)

    valueOrFile inline file fileContents options =
      case (optionValueFor inline options, optionValueFor file options) of
        (Just value, Nothing) -> Right value
        (Nothing, Just path) ->
          case lookup path fileContents of
            Just value -> Right value
            Nothing -> Left ("missing contents for " ++ file ++ ": " ++ path ++ "\n" ++ llmUsage)
        (Just _, Just _) ->
          Left ("use only one of " ++ inline ++ " and " ++ file ++ "\n" ++ llmUsage)
        (Nothing, Nothing) ->
          Left ("missing required argument: " ++ inline ++ " or " ++ file ++ "\n" ++ llmUsage)

    policyOption options =
      case optionValueFor "--model-shell" options of
        Nothing -> Right ModelShellDenied
        Just value ->
          case modelShellFromArgument value of
            Just policy -> Right policy
            Nothing ->
              Left
                ( "invalid value for --model-shell: "
                    ++ value
                    ++ " (expected deny, ask, or allow)\n"
                    ++ llmUsage
                )

    withUsage result =
      case result of
        Left message -> Left (message ++ "\n" ++ llmUsage)
        Right options -> Right options

-- | File paths the caller must read before 'parseLlmSettingsWithFiles'. The
-- paths are deduplicated so a caller can point both options at one file.
llmFilePaths :: [String] -> Either String [FilePath]
llmFilePaths arguments =
  case parseOptions llmOptionSpec arguments of
    Left message -> Left (message ++ "\n" ++ llmUsage)
    Right options ->
      Right . nub $
        catMaybes
          [ optionValueFor "--model-file" options
          , optionValueFor "--system-prompt-file" options
          ]

llmOptionSpec :: [(String, OptionKind)]
llmOptionSpec =
  [ ("--llama-url", OptionValue)
  , ("--model", OptionValue)
  , ("--model-file", OptionValue)
  , ("--system-prompt", OptionValue)
  , ("--system-prompt-file", OptionValue)
  , ("--llama-socket", OptionValue)
  , ("--model-shell", OptionValue)
  ]

-- | Reject malformed URLs, empty model names, and empty socket paths; an empty
-- system prompt is allowed.
settingsFrom :: LlmUrl -> ModelName -> SystemPrompt -> Maybe FilePath -> ModelShell -> Either String LlmSettings
settingsFrom url model systemPrompt socket policy = do
  validateLlmUrl url
  validateNonEmpty "--model" (unModelName model)
  maybe (Right ()) (validateNonEmpty "--llama-socket") socket
  pure (LlmSettings url model systemPrompt socket policy)
  where
    validateNonEmpty name value
      | null value = Left ("empty value for " ++ name ++ "\n" ++ llmUsage)
      | otherwise = Right ()

-- | A URL must use http or https, name a host, and, if it has a path, start
-- that path with a slash. Validation happens before any request is attempted.
validateLlmUrl :: LlmUrl -> Either String ()
validateLlmUrl (LlmUrl url)
  | null url = invalid "empty value for --llama-url"
  | any invalidChar url = invalid "invalid character in --llama-url"
  | otherwise =
      case stripScheme url of
        Nothing -> invalid "invalid value for --llama-url: expected an http:// or https:// URL"
        Just rest
          | null (authorityOf rest) ->
              invalid "invalid value for --llama-url: missing host"
          | not (null path) && take 1 path /= "/" ->
              invalid "invalid value for --llama-url: the path must start with /"
          | otherwise -> Right ()
  where
    invalid message = Left (message ++ "\n" ++ llmUsage)
    invalidChar character = isSpace character || isControl character
    path = urlPath (LlmUrl url)

stripScheme :: String -> Maybe String
stripScheme text
  | "http://" `isPrefixOf` text = Just (drop (length ("http://" :: String)) text)
  | "https://" `isPrefixOf` text = Just (drop (length ("https://" :: String)) text)
  | otherwise = Nothing

-- | The authority part of a URL after its scheme.
authorityOf :: String -> String
authorityOf = takeWhile (`notElem` ['/', '?', '#'])

-- | The path of a URL without its query or fragment.
urlPath :: LlmUrl -> String
urlPath (LlmUrl url) =
  takeWhile (`notElem` ['?', '#']) . dropWhile (`notElem` ['/', '?', '#']) $ url

missingMessage :: [String] -> String
missingMessage missing =
  "missing required argument"
    ++ plural
    ++ ": "
    ++ unwords missing
    ++ "\n"
    ++ llmUsage
  where
    plural = if length missing > 1 then "s" else ""

llmUsage :: String
llmUsage =
  "usage: hutian-shell --llama-url URL (--model MODEL | --model-file PATH) (--system-prompt PROMPT | --system-prompt-file PATH) [--llama-socket PATH] [--model-shell deny|ask|allow]"
