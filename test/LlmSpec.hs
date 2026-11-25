{-# LANGUAGE OverloadedStrings #-}

module LlmSpec (tests) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Hutian.Conversation (ConversationEntry (..), ToolCall (..), emptyConversation)
import Hutian.Llm
import Hutian.Policy (ToolPolicy (..))
import Hutian.Tool.Catalog (ToolCatalog, toolCatalog)
import Hutian.Tool.Config (AgentConfig (..), SubagentConfig (..), ToolConfig (..), defaultToolConfig)
import Hutian.Verbosity (Verbosity (..))
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.IO (hClose, hPutStr, openTempFile)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

shellTools :: ToolPolicy -> ToolCatalog
shellTools policy = toolCatalog (defaultToolConfig {toolShell = policy})

testSettings :: Api -> ToolCatalog -> String -> String -> LlmSettings
testSettings api tools model systemPrompt =
  LlmSettings
    { settingUrl = LlmUrl url
    , settingModel = ModelName model
    , settingSystemPrompt = SystemPrompt systemPrompt
    , settingSocket = Nothing
    , settingVerbosity = VerbosityHigh
    , settingTools = tools
    }
  where
    url =
      case api of
        ChatApi -> "http://example.test/v1/chat/completions"
        CompletionApi -> "http://example.test/completion"

parseSettings :: [String] -> Either String LlmSettings
parseSettings = parseSettingsWithVerbosity "--verbosity" "high"

parseSettingsWithVerbosity :: String -> String -> [String] -> Either String LlmSettings
parseSettingsWithVerbosity flag value =
  parseLlmSettings . ([flag, value] ++)

tests :: TestTree
tests =
  testGroup
    "llm"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "parseSettings accepts the required options in any order" $ do
        parseSettings
          [ "--system-prompt"
          , "system rules"
          , "--model"
          , "test-model"
          , "--llama-url"
          , "http://127.0.0.1:8080/v1/chat/completions"
          ]
          @?= Right
            ( LlmSettings
                (LlmUrl "http://127.0.0.1:8080/v1/chat/completions")
                (ModelName "test-model")
                (SystemPrompt "system rules")
                Nothing
                VerbosityHigh
                (shellTools ToolDeny)
            )
        parseSettings
          [ "--llama-url"
          , "http://example.test/completion"
          , "--model"
          , "m"
          , "--system-prompt"
          , ""
          ]
          @?= Right
            (LlmSettings (LlmUrl "http://example.test/completion") (ModelName "m") (SystemPrompt "") Nothing VerbosityHigh (shellTools ToolDeny))
    , testCase "parseLlmLaunchWithFiles returns positional prompt arguments" $ do
        let base =
              [ "--llama-url"
              , "http://example.test/v1/chat/completions"
              , "--verbosity"
              , "high"
              , "--model"
              , "m"
              , "--system-prompt"
              , ""
              ]
        case parseLlmLaunchWithFiles [] (base ++ ["hello", "world"]) of
          Left message -> assertFailure message
          Right (_, promptWords) -> promptWords @?= ["hello", "world"]
        case parseLlmLaunchWithFiles [] (base ++ ["--", "--looks-like-a-flag"]) of
          Left message -> assertFailure message
          Right (_, promptWords) -> promptWords @?= ["--looks-like-a-flag"]
        case parseLlmLaunchWithFiles [] (base ++ ["--unknown"]) of
          Left message -> assertBool "unknown option should be named" ("--unknown" `isInfixOf` message)
          Right _ -> assertFailure "unknown options must still fail"
    , testCase "parseLlmSettings rejects positional arguments" $
        case
            parseLlmSettings
              [ "--llama-url"
              , "http://example.test/v1/chat/completions"
              , "--verbosity"
              , "high"
              , "--model"
              , "m"
              , "--system-prompt"
              , ""
              , "prompt"
              ]
          of
            Left message -> assertBool "positional should be named" ("unknown argument: prompt" `isInfixOf` message)
            Right _ -> assertFailure "positional arguments must fail in strict parsing"
    , testCase "parseSettings accepts an optional relay socket" $ do
        parseSettings
          [ "--llama-url"
          , "http://example.test/v1/chat/completions"
          , "--model"
          , "m"
          , "--system-prompt"
          , "system rules"
          , "--llama-socket"
          , "/run/hutian/llm.sock"
          ]
          @?= Right
            ( LlmSettings
                (LlmUrl "http://example.test/v1/chat/completions")
                (ModelName "m")
                (SystemPrompt "system rules")
                (Just "/run/hutian/llm.sock")
                VerbosityHigh
                (shellTools ToolDeny)
            )
    , testCase "parseLlmSettingsWithFiles reads model and prompt contents" $
        parseLlmSettingsWithFiles
          [ ("/run/model", "file-model")
          , ("/run/prompt", "file rules")
          ]
          [ "--llama-url"
          , "http://example.test/v1/chat/completions"
          , "--model-file"
          , "/run/model"
          , "--system-prompt-file"
          , "/run/prompt"
          , "--verbosity"
          , "high"
          ]
          @?= Right
            ( LlmSettings
                (LlmUrl "http://example.test/v1/chat/completions")
                (ModelName "file-model")
                (SystemPrompt "file rules")
                Nothing
                VerbosityHigh
                (shellTools ToolDeny)
            )
    , testCase "loadLlmLaunch removes the model and prompt files" $ do
        temporary <- getTemporaryDirectory
        (modelPath, modelHandle) <- openTempFile temporary "hutian-model"
        hPutStr modelHandle "file-model"
        hClose modelHandle
        (promptPath, promptHandle) <- openTempFile temporary "hutian-prompt"
        hPutStr promptHandle "file rules"
        hClose promptHandle
        result <-
          loadLlmLaunch
            [ "--llama-url"
            , "http://example.test/v1/chat/completions"
            , "--model-file"
            , modelPath
            , "--system-prompt-file"
            , promptPath
            , "--verbosity"
            , "high"
            ]
        fmap fst result
          @?= Right
            ( LlmSettings
                (LlmUrl "http://example.test/v1/chat/completions")
                (ModelName "file-model")
                (SystemPrompt "file rules")
                Nothing
                VerbosityHigh
                (shellTools ToolDeny)
            )
        modelExists <- doesFileExist modelPath
        promptExists <- doesFileExist promptPath
        assertBool "the model file should be removed" (not modelExists)
        assertBool "the system prompt file should be removed" (not promptExists)
    , testCase "parseLlmSettingsWithFiles reads a standalone subagent object" $
        parseLlmSettingsWithFiles
          [ ( "/run/subagents"
            , "{\"cheap\":{\"model\":\"cheap-upstream\",\"description\":\"Summarize pages.\",\"systemPrompt\":\"Summarize.\",\"tools\":{\"shell\":\"deny\",\"webfetch\":\"allow\",\"subagent\":{},\"mcp\":{}}}}"
            )
          ]
          [ "--llama-url"
          , "http://example.test/v1/chat/completions"
          , "--model"
          , "m"
          , "--system-prompt"
          , ""
          , "--verbosity"
          , "high"
          , "--subagent-file"
          , "/run/subagents"
          ]
          @?= Right
            ( LlmSettings
                (LlmUrl "http://example.test/v1/chat/completions")
                (ModelName "m")
                (SystemPrompt "")
                Nothing
                VerbosityHigh
                ( toolCatalog
                    ( defaultToolConfig
                        { toolShell = ToolDeny
                        , toolSubagent =
                            [ SubagentConfig
                                { subagentName = "cheap"
                                , subagentAgent =
                                    AgentConfig
                                      { agentModel = "cheap-upstream"
                                      , agentDescription = "Summarize pages."
                                      , agentSystemPrompt = "Summarize."
                                      , agentTools = defaultToolConfig {toolWebfetch = ToolAllow}
                                      }
                                }
                            ]
                        }
                    )
                )
            )
    , testCase "parseLlmSettings parses verbosity" $ do
        let parseVerbosity extra =
              parseLlmSettings
                ( [ "--llama-url"
                  , "http://example.test/v1/chat/completions"
                  , "--model"
                  , "m"
                  , "--system-prompt"
                  , ""
                  ]
                    ++ extra
                )
            verbosityOf result =
              case result of
                Right settings -> Just (settingVerbosity settings)
                Left _ -> Nothing
        case parseVerbosity [] of
          Left message -> assertBool "missing verbosity named" ("--verbosity" `isInfixOf` message)
          Right _ -> assertFailure "verbosity is mandatory"
        verbosityOf (parseVerbosity ["--verbosity", "low"]) @?= Just VerbosityLow
        verbosityOf (parseVerbosity ["--verbosity", "normal"]) @?= Just VerbosityNormal
        verbosityOf (parseVerbosity ["--verbosity", "high"]) @?= Just VerbosityHigh
        case parseVerbosity ["--verbosity", "loud"] of
          Left message -> assertBool "names the option" ("--verbosity" `isInfixOf` message)
          Right _ -> assertFailure "invalid verbosity must be rejected"
    , testCase "parseSettings parses each model shell policy" $ do
        let parsePolicy extra =
              parseSettings
                ( [ "--llama-url"
                  , "http://example.test/v1/chat/completions"
                  , "--model"
                  , "m"
                  , "--system-prompt"
                  , ""
                  ]
                    ++ extra
                )
            settings policy =
              Right
                ( LlmSettings
                    (LlmUrl "http://example.test/v1/chat/completions")
                    (ModelName "m")
                    (SystemPrompt "")
                    Nothing
                    VerbosityHigh
                    (shellTools policy)
                )
        parsePolicy [] @?= settings ToolDeny
        parsePolicy ["--model-shell", "deny"] @?= settings ToolDeny
        parsePolicy ["--model-shell", "ask"] @?= settings ToolAsk
        parsePolicy ["--model-shell", "allow"] @?= settings ToolAllow
    , testCase "parseSettings rejects an empty relay socket" $
        case
            parseSettings
              [ "--llama-url"
              , "http://example.test/v1/chat/completions"
              , "--model"
              , "m"
              , "--system-prompt"
              , ""
              , "--llama-socket"
              , ""
              ]
          of
            Left message -> assertBool "empty socket should be named" ("--llama-socket" `isInfixOf` message)
            Right _ -> assertFailure "an empty socket should fail"
    , testCase "parseSettings rejects empty URLs and model names" $ do
        case parseSettings ["--llama-url", "", "--model", "m", "--system-prompt", ""] of
          Left message -> assertBool "empty url should be named" ("--llama-url" `isInfixOf` message)
          Right _ -> assertFailure "an empty url should fail"
        case parseSettings ["--llama-url", "http://example.test", "--model", "", "--system-prompt", ""] of
          Left message -> assertBool "empty model should be named" ("--model" `isInfixOf` message)
          Right _ -> assertFailure "an empty model should fail"
    , testCase "parseSettings rejects malformed URLs" $ do
        assertUrlRejected "not-a-url"
        assertUrlRejected "ftp://example.test/completion"
        assertUrlRejected "http:///v1/chat/completions"
        assertUrlRejected "http://example.test/a path"
    , testCase "parseSettings reports missing options, missing values, and unknown arguments" $ do
        case parseLlmSettings [] of
          Left message -> do
            assertBool "missing url should be named" ("--llama-url" `isInfixOf` message)
            assertBool "missing verbosity should be named" ("--verbosity" `isInfixOf` message)
            assertBool "missing model should be named" ("--model" `isInfixOf` message)
            assertBool "missing prompt should be named" ("--system-prompt" `isInfixOf` message)
          Right _ -> assertFailure "empty arguments should fail"
        case parseLlmSettings ["--llama-url"] of
          Left message -> assertBool "missing value should be named" ("--llama-url" `isInfixOf` message)
          Right _ -> assertFailure "a flag without a value should fail"
        case parseLlmSettings ["--llama-socket"] of
          Left message -> assertBool "missing socket value should be named" ("--llama-socket" `isInfixOf` message)
          Right _ -> assertFailure "a socket flag without a value should fail"
        case parseLlmSettings ["--model-shell"] of
          Left message -> assertBool "missing policy value should be named" ("--model-shell" `isInfixOf` message)
          Right _ -> assertFailure "a model shell flag without a value should fail"
        case
            parseSettings
              [ "--llama-url"
              , "http://example.test"
              , "--model"
              , "m"
              , "--system-prompt"
              , ""
              , "--model-shell"
              , "sometimes"
              ]
          of
            Left message -> assertBool "invalid policy should be named" ("--model-shell" `isInfixOf` message)
            Right _ -> assertFailure "an invalid model shell policy should fail"
        case parseSettings ["--unknown", "value"] of
          Left message -> assertBool "unknown argument should be named" ("--unknown" `isInfixOf` message)
          Right _ -> assertFailure "unknown arguments should fail"
        case
            parseSettings
              [ "--llama-url"
              , "http://example.test"
              , "--model"
              , "one"
              , "--model"
              , "two"
              , "--system-prompt"
              , ""
              ]
          of
            Left message -> assertBool "duplicate option should be named" ("duplicate argument" `isInfixOf` message)
            Right _ -> assertFailure "duplicate options should fail"
    , testCase "apiFor recognizes chat and completion endpoints" $ do
        apiFor (LlmUrl "http://127.0.0.1:8080/v1/chat/completions") @?= ChatApi
        apiFor (LlmUrl "http://127.0.0.1:8080/completion") @?= CompletionApi
        apiFor (LlmUrl "https://example.test/v1/chat/completions?stream=true") @?= ChatApi
        apiFor (LlmUrl "https://example.test/api/v1/chat/completions") @?= ChatApi
        apiFor (LlmUrl "https://example.test/v1/chat/completions-extra") @?= CompletionApi
        apiFor (LlmUrl "https://example.test/v1/chat/completions/") @?= ChatApi
    , testCase "chat request body has system and user messages" $
        case requestBodyFor (testSettings ChatApi (shellTools ToolDeny) "test-model" "system rules") emptyConversation "hello" of
          Object body -> do
            KeyMap.lookup "model" body @?= Just (String "test-model")
            KeyMap.lookup "stream" body @?= Just (Bool True)
            case KeyMap.lookup "messages" body of
              Just (Array messages) ->
                case Vector.toList messages of
                  [Object systemMessage, Object userMessage] -> do
                    KeyMap.lookup "role" systemMessage @?= Just (String "system")
                    KeyMap.lookup "content" systemMessage @?= Just (String "system rules")
                    KeyMap.lookup "role" userMessage @?= Just (String "user")
                    KeyMap.lookup "content" userMessage @?= Just (String "hello")
                  other -> assertFailure ("unexpected messages: " ++ show other)
              other -> assertFailure ("missing messages: " ++ show other)
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request includes prior conversation and terminal history" $
        case
            requestBodyFor
              (testSettings ChatApi (shellTools ToolDeny) "test-model" "system rules")
              [UserEntry "previous question", AssistantEntry "previous answer", TerminalEntry "ls -la"]
              "new question"
          of
            Object body ->
              case KeyMap.lookup "messages" body of
                Just (Array messages) ->
                  case Vector.toList messages of
                    [Object systemMessage, Object user1, Object assistant1, Object terminal, Object user2] -> do
                      KeyMap.lookup "role" systemMessage @?= Just (String "system")
                      KeyMap.lookup "role" user1 @?= Just (String "user")
                      KeyMap.lookup "content" user1 @?= Just (String "previous question")
                      KeyMap.lookup "role" assistant1 @?= Just (String "assistant")
                      KeyMap.lookup "content" assistant1 @?= Just (String "previous answer")
                      KeyMap.lookup "role" terminal @?= Just (String "user")
                      assertBool
                        "terminal command should be in the message"
                        (maybe False (containsText "ls -la") (KeyMap.lookup "content" terminal))
                      KeyMap.lookup "role" user2 @?= Just (String "user")
                      KeyMap.lookup "content" user2 @?= Just (String "new question")
                    other -> assertFailure ("unexpected messages: " ++ show other)
                other -> assertFailure ("missing messages: " ++ show other)
            other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request body omits an empty system message" $
        case requestBodyFor (testSettings ChatApi (shellTools ToolDeny) "test-model" "") emptyConversation "hello" of
          Object body ->
            case KeyMap.lookup "messages" body of
              Just (Array messages) -> Vector.length messages @?= 1
              other -> assertFailure ("missing messages: " ++ show other)
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "completion request body prepends the system prompt" $
        case requestBodyFor (testSettings CompletionApi (shellTools ToolDeny) "test-model" "system rules") emptyConversation "hello" of
          Object body -> do
            KeyMap.lookup "model" body @?= Just (String "test-model")
            KeyMap.lookup "prompt" body @?= Just (String "System: system rules\n\nUser: hello\n\nAssistant:")
            KeyMap.lookup "stream" body @?= Just (Bool True)
            KeyMap.lookup "messages" body @?= Nothing
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request body advertises the shell and job tools when allowed" $
        case requestBodyFor (testSettings ChatApi (shellTools ToolAllow) "test-model" "") emptyConversation "hello" of
          Object body ->
            case KeyMap.lookup "tools" body of
              Just (Array tools) ->
                map toolName (Vector.toList tools)
                  @?= ["shell", "shell_background", "jobs", "job_output", "job_kill"]
              other -> assertFailure ("missing tools: " ++ show other)
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request body advertises the shell tool in ask mode" $
        case requestBodyFor (testSettings ChatApi (shellTools ToolAsk) "test-model" "") emptyConversation "hello" of
          Object body -> do
            assertBool "ask mode should advertise tools" (KeyMap.member "tools" body)
            KeyMap.lookup "tool_choice" body @?= Just (String "auto")
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request body omits the shell tool when denied" $
        case requestBodyFor (testSettings ChatApi (shellTools ToolDeny) "test-model" "") emptyConversation "hello" of
          Object body -> do
            KeyMap.lookup "tools" body @?= Nothing
            KeyMap.lookup "tool_choice" body @?= Nothing
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "extractToolCalls reads streamed fragments" $ do
        let line =
              "data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call-1\",\"type\":\"function\",\"function\":{\"name\":\"shell\",\"arguments\":\"{\\\"command\\\":\"}}]}}]}"
        case parseStreamLine line of
          Just event ->
            eventToolCalls event
              @?= [ToolCallDelta 0 (Just "call-1") (Just "shell") (Just "{\"command\":")]
          Nothing -> assertFailure "tool call event did not parse"
    , testCase "mergeToolCallDeltas assembles streamed arguments" $ do
        let first = ToolCallDelta 0 (Just "call-1") (Just "shell") (Just "{\"command\":")
            second = ToolCallDelta 0 Nothing Nothing (Just "\"ls\"}")
        IntMap.elems (mergeToolCallDeltas IntMap.empty [first, second])
          @?= [ToolCall "call-1" "shell" "{\"command\":\"ls\"}"]
    , testCase "parseStreamLine parses SSE and bare JSON lines" $ do
        parseStreamLine "" @?= Nothing
        parseStreamLine "   " @?= Nothing
        parseStreamLine "data: [DONE]" @?= Just (StreamEvent Nothing Nothing Nothing [] True)
        parseStreamLine "data: [DONE]\r" @?= Just (StreamEvent Nothing Nothing Nothing [] True)
        parseStreamLine "data: {\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}"
          @?= Just (StreamEvent Nothing (Just "hi") Nothing [] False)
        parseStreamLine "data: {\"reasoning_content\":\"thinking\"}"
          @?= Just (StreamEvent (Just "thinking") Nothing Nothing [] False)
        parseStreamLine "data: {\"error\":\"boom\"}"
          @?= Just (StreamEvent Nothing Nothing (Just "boom") [] False)
        parseStreamLine "data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
          @?= Just (StreamEvent Nothing Nothing Nothing [] True)
        parseStreamLine "{\"content\":\"direct\"}"
          @?= Just (StreamEvent Nothing (Just "direct") Nothing [] False)
        parseStreamLine "data: not-json" @?= Nothing
    , testCase "splitLines keeps partial data in the remainder" $ do
        splitLines "a\nb\n" @?= (["a", "b"], "")
        splitLines "a\nb" @?= (["a"], "b")
        splitLines "" @?= ([], "")
        splitLines "\n" @?= ([""], "")
    , testCase "extractContent handles direct and delta content" $ do
        extractContent (KeyMap.fromList [("content", String "direct")]) @?= Just "direct"
        case object ["choices" .= [object ["delta" .= object ["content" .= ("delta" :: Text.Text)]]]] of
          Object value -> extractContent value @?= Just "delta"
          _ -> assertFailure "choices value is not an object"
        extractContent (KeyMap.fromList [("other", String "x")]) @?= Nothing
    , testCase "extractReasoning handles reasoning fields and deltas" $ do
        extractReasoning (KeyMap.fromList [("reasoning_content", String "thought")]) @?= Just "thought"
        extractReasoning (KeyMap.fromList [("reasoning", String "thought")]) @?= Just "thought"
        extractReasoning (KeyMap.fromList [("thinking", String "thought")]) @?= Just "thought"
        case object ["choices" .= [object ["delta" .= object ["reasoning_content" .= ("delta thought" :: Text.Text)]]]] of
          Object value -> extractReasoning value @?= Just "delta thought"
          _ -> assertFailure "choices value is not an object"
        extractReasoning (KeyMap.fromList [("content", String "answer")]) @?= Nothing
    , testCase "extractError handles string and nested errors" $ do
        extractError (KeyMap.fromList [("error", String "boom")]) @?= Just "boom"
        case object ["error" .= object ["message" .= ("nested" :: Text.Text)]] of
          Object value -> extractError value @?= Just "nested"
          _ -> assertFailure "error value is not an object"
        extractError (KeyMap.fromList [("error", Null)]) @?= Nothing
    , testCase "isStop handles stop and finish_reason" $ do
        isStop (KeyMap.fromList [("stop", Bool True)]) @?= True
        case object ["choices" .= [object ["finish_reason" .= ("stop" :: Text.Text)]]] of
          Object value -> isStop value @?= True
          _ -> assertFailure "choices value is not an object"
        case object ["choices" .= [object ["finish_reason" .= Null]]] of
          Object value -> isStop value @?= False
          _ -> assertFailure "choices value is not an object"
        isStop (KeyMap.fromList []) @?= False
    , testCase "decode replaces invalid UTF-8 leniently" $ do
        decode (BS.pack [0x68, 0x69]) @?= "hi"
        decode (BS.pack [0xff]) @?= "\xFFFD"
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "splitLines round-trips arbitrary bytes" prop_splitLinesRoundTrip
    , testProperty "splitLines leaves no newlines in its output" prop_splitLinesNoNewline
    , testProperty "apiFor classifies by chat path" prop_apiFor
    , testProperty "extractContent finds direct content" prop_extractContent
    , testProperty "extractError finds nested messages" prop_extractError
    , testProperty "request bodies always name the model and stream" prop_requestBody
    , testProperty "settings round-trip through the command line" prop_parseSettings
    , testProperty "settings round-trip with a relay socket" prop_parseSettingsWithSocket
    ]

-- * Properties

prop_splitLinesRoundTrip :: [Word8] -> Property
prop_splitLinesRoundTrip bytes =
  counterexample (show (input, chunks, remainder)) $
    rebuilt === input
  where
    input = BS.pack bytes
    (chunks, remainder) = splitLines input
    rebuilt =
      BS.intercalate "\n" chunks
        <> (if null chunks then "" else "\n")
        <> remainder

prop_splitLinesNoNewline :: [Word8] -> Property
prop_splitLinesNoNewline bytes =
  counterexample (show (chunks, remainder)) $
    property (not (any (BS.elem 10) chunks))
      .&&. property (not (BS.elem 10 remainder))
  where
    (chunks, remainder) = splitLines (BS.pack bytes)

prop_apiFor :: String -> Property
prop_apiFor url =
  apiFor (LlmUrl url) === expected
  where
    expected
      | "/v1/chat/completions" `isSuffixOf` dropTrailingSlash (requestPath url) = ChatApi
      | otherwise = CompletionApi
    requestPath =
      takeWhile (`notElem` ['?', '#']) . dropWhile (`notElem` ['/', '?', '#'])
    dropTrailingSlash path =
      case reverse path of
        '/' : rest -> reverse rest
        _ -> path

prop_parseSettings :: NonEmptyList Char -> String -> Property
prop_parseSettings model systemPrompt =
  parseSettings
    [ "--llama-url"
    , urlText
    , "--model"
    , modelText
    , "--system-prompt"
    , systemPrompt
    ]
    === Right
      ( LlmSettings
          (LlmUrl urlText)
          (ModelName modelText)
          (SystemPrompt systemPrompt)
          Nothing
          VerbosityHigh
          (shellTools ToolDeny)
      )
  where
    urlText = "http://example.test/v1/chat/completions"
    modelText = getNonEmpty model

prop_parseSettingsWithSocket :: NonEmptyList Char -> String -> NonEmptyList Char -> Property
prop_parseSettingsWithSocket model systemPrompt socket =
  parseSettings
    [ "--llama-url"
    , urlText
    , "--llama-socket"
    , socketText
    , "--model"
    , modelText
    , "--system-prompt"
    , systemPrompt
    ]
    === Right
      ( LlmSettings
          (LlmUrl urlText)
          (ModelName modelText)
          (SystemPrompt systemPrompt)
          (Just socketText)
          VerbosityHigh
          (shellTools ToolDeny)
      )
  where
    urlText = "http://example.test/v1/chat/completions"
    modelText = getNonEmpty model
    socketText = getNonEmpty socket

prop_extractContent :: String -> Property
prop_extractContent text =
  extractContent (KeyMap.fromList [("content", String (Text.pack text))]) === Just (Text.pack text)

prop_extractError :: String -> Property
prop_extractError text =
  case object ["error" .= object ["message" .= Text.pack text]] of
    Object value -> extractError value === Just (Text.pack text)
    _ -> counterexample "test value is not an object" False

containsText :: String -> Value -> Bool
containsText needle (String text) = Text.pack needle `Text.isInfixOf` text
containsText _ _ = False

assertUrlRejected :: String -> IO ()
assertUrlRejected url =
  case parseSettings ["--llama-url", url, "--model", "m", "--system-prompt", ""] of
    Left message -> assertBool ("URL should be named in: " ++ message) ("--llama-url" `isInfixOf` message)
    Right _ -> assertFailure ("malformed URL should fail: " ++ url)

prop_requestBody :: String -> String -> String -> Property
prop_requestBody model systemPrompt text =
  forAll (elements [ChatApi, CompletionApi]) $ \api ->
    counterexample (show (api, model, systemPrompt, text)) $
      case Aeson.decode (Aeson.encode (requestBodyFor (testSettings api (shellTools ToolAllow) model systemPrompt) emptyConversation text)) of
        Just (Object body) ->
          KeyMap.lookup "model" body === Just (String (Text.pack model))
            .&&. KeyMap.lookup "stream" body === Just (Bool True)
        _ -> counterexample "request body is not an object" False

toolName :: Value -> Text.Text
toolName (Object tool) =  case KeyMap.lookup "function" tool of
    Just (Object functionObject) ->
      case KeyMap.lookup "name" functionObject of
        Just (String name) -> name
        _ -> ""
    _ -> ""
toolName _ = ""
