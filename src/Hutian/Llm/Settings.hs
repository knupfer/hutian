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

import qualified Data.Aeson as Aeson
import Data.Char (isControl, isSpace)
import Data.List (isPrefixOf, nub)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Hutian.Cli
  ( OptionKind (..)
  , hasOption
  , missingOptions
  , optionValueFor
  , parseOptions
  )
import Hutian.Policy (ToolPolicy (..), toolPolicyFromArgument)
import Hutian.Tool.Catalog (ToolCatalog, toolCatalog)
import Hutian.Tool.Config
  ( SubagentConfigs (..)
  , ToolConfig (..)
  , WebsearchConfig (..)
  , defaultToolConfig
  )
import Hutian.Verbosity (Verbosity (..), verbosityFromArgument)

-- | Connection settings supplied by the launcher.
data LlmSettings = LlmSettings
  { settingUrl :: LlmUrl
  , settingModel :: ModelName
  , settingSystemPrompt :: SystemPrompt
  , settingSocket :: Maybe FilePath
  , settingVerbosity :: Verbosity
  -- | The model's tool policies, nested subagent configuration, and discovered
  -- MCP tools. Term mode is unaffected.
  , settingTools :: ToolCatalog
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
        missingOptions ["--llama-url", "--verbosity"] options
          ++ missingOption "--model" "--model-file"
          ++ missingOption "--system-prompt" "--system-prompt-file"
  case missing of
    [] -> do
      url <- required "--llama-url" options
      model <- valueOrFile "--model" "--model-file" loaded options
      systemPrompt <- valueOrFile "--system-prompt" "--system-prompt-file" loaded options
      verbosityValue <- required "--verbosity" options
      verbosity <-
        case verbosityFromArgument verbosityValue of
          Just value -> Right value
          Nothing ->
            Left
              ( "invalid value for --verbosity: "
                  ++ verbosityValue
                  ++ " (expected low, normal, or high)\n"
                  ++ llmUsage
              )
      tools <- toolsOption loaded options
      settingsFrom
        (LlmUrl url)
        (ModelName model)
        (SystemPrompt systemPrompt)
        (optionValueFor "--llama-socket" options)
        verbosity
        tools
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

    toolsOption fileContents options =
      case optionValueFor "--tools-file" options of
        Just path ->
          case lookup path fileContents of
            Just contents -> decodeFile "--tools-file" path contents
            Nothing -> Left ("missing contents for --tools-file: " ++ path ++ "\n" ++ llmUsage)
        Nothing -> do
          shell <- policyOption "--model-shell" ToolDeny options
          webfetch <- policyOption "--model-webfetch" ToolDeny options
          websearch <- websearchOption options
          subagent <- subagentsOption fileContents options
          pure
            defaultToolConfig
              { toolShell = shell
              , toolWebfetch = webfetch
              , toolWebsearch = websearch
              , toolSubagent = subagent
              }

    policyOption flag fallback options =
      case optionValueFor flag options of
        Nothing -> Right fallback
        Just value ->
          case toolPolicyFromArgument value of
            Just policy -> Right policy
            Nothing ->
              Left
                ( "invalid value for "
                    ++ flag
                    ++ ": "
                    ++ value
                    ++ " (expected deny, ask, or allow)\n"
                    ++ llmUsage
                )

    websearchOption options =
      case optionValueFor "--websearch-url" options of
        Nothing -> Right Nothing
        Just url -> do
          policy <- policyOption "--model-websearch" ToolAsk options
          pure (Just (WebsearchConfig url policy))

    subagentsOption fileContents options =
      case optionValueFor "--subagent-file" options of
        Nothing -> Right []
        Just path ->
          case lookup path fileContents of
            Just contents ->
              unSubagentConfigs <$> decodeFile "--subagent-file" path contents
            Nothing -> Left ("missing contents for --subagent-file: " ++ path ++ "\n" ++ llmUsage)

    withUsage result =
      case result of
        Left message -> Left (message ++ "\n" ++ llmUsage)
        Right options -> Right options

decodeFile :: Aeson.FromJSON value => String -> FilePath -> String -> Either String value
decodeFile flag path contents =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 (Text.pack contents)) of
    Right value -> Right value
    Left message -> Left (invalidFile flag path message)

invalidFile :: String -> FilePath -> String -> String
invalidFile flag path message =
  "invalid "
    ++ flag
    ++ " file "
    ++ path
    ++ ": "
    ++ message
    ++ "\n"
    ++ llmUsage

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
          , optionValueFor "--tools-file" options
          , optionValueFor "--subagent-file" options
          ]

llmOptionSpec :: [(String, OptionKind)]
llmOptionSpec =
  [ ("--llama-url", OptionValue)
  , ("--model", OptionValue)
  , ("--model-file", OptionValue)
  , ("--system-prompt", OptionValue)
  , ("--system-prompt-file", OptionValue)
  , ("--llama-socket", OptionValue)
  , ("--verbosity", OptionValue)
  , ("--model-shell", OptionValue)
  , ("--model-webfetch", OptionValue)
  , ("--websearch-url", OptionValue)
  , ("--model-websearch", OptionValue)
  , ("--tools-file", OptionValue)
  , ("--subagent-file", OptionValue)
  ]

-- | Reject malformed URLs, empty model names, empty socket paths, and an empty
-- search endpoint; an empty system prompt is allowed.
settingsFrom :: LlmUrl -> ModelName -> SystemPrompt -> Maybe FilePath -> Verbosity -> ToolConfig -> Either String LlmSettings
settingsFrom url model systemPrompt socket verbosity tools = do
  validateLlmUrl url
  validateNonEmpty "--model" (unModelName model)
  maybe (Right ()) (validateNonEmpty "--llama-socket") socket
  validateWebsearch tools
  pure (LlmSettings url model systemPrompt socket verbosity (toolCatalog tools))
  where
    validateNonEmpty name value
      | null value = Left ("empty value for " ++ name ++ "\n" ++ llmUsage)
      | otherwise = Right ()

    validateWebsearch config =
      case toolWebsearch config of
        Just websearch
          | null (websearchUrl websearch) ->
              Left ("empty value for the websearch URL\n" ++ llmUsage)
        _ -> Right ()

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
  "usage: hutian-shell --llama-url URL --verbosity low|normal|high (--model MODEL | --model-file PATH) (--system-prompt PROMPT | --system-prompt-file PATH) [--llama-socket PATH] [--tools-file PATH] [--model-shell deny|ask|allow] [--model-webfetch deny|ask|allow] [--websearch-url URL] [--model-websearch deny|ask|allow] [--subagent-file PATH]"
