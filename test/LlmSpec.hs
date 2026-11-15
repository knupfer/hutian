{-# LANGUAGE OverloadedStrings #-}

module LlmSpec (tests) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Word (Word8)
import Hutian.Conversation (ConversationEntry (..), ToolCall (..), emptyConversation)
import Hutian.Llm
import Hutian.Policy (ModelShell (..))
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.IO (hClose, hPutStr, openTempFile)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

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
    [ testCase "parseLlmSettings accepts the required options in any order" $ do
        parseLlmSettings
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
                ModelShellDenied
            )
        parseLlmSettings
          [ "--llama-url"
          , "http://example.test/completion"
          , "--model"
          , "m"
          , "--system-prompt"
          , ""
          ]
          @?= Right
            (LlmSettings (LlmUrl "http://example.test/completion") (ModelName "m") (SystemPrompt "") Nothing ModelShellDenied)
    , testCase "parseLlmSettings accepts an optional relay socket" $ do
        parseLlmSettings
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
                ModelShellDenied
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
          ]
          @?= Right
            ( LlmSettings
                (LlmUrl "http://example.test/v1/chat/completions")
                (ModelName "file-model")
                (SystemPrompt "file rules")
                Nothing
                ModelShellDenied
            )
    , testCase "loadLlmSettings removes the model and prompt files" $ do
        temporary <- getTemporaryDirectory
        (modelPath, modelHandle) <- openTempFile temporary "hutian-model"
        hPutStr modelHandle "file-model"
        hClose modelHandle
        (promptPath, promptHandle) <- openTempFile temporary "hutian-prompt"
        hPutStr promptHandle "file rules"
        hClose promptHandle
        result <-
          loadLlmSettings
            [ "--llama-url"
            , "http://example.test/v1/chat/completions"
            , "--model-file"
            , modelPath
            , "--system-prompt-file"
            , promptPath
            ]
        result
          @?= Right
            ( LlmSettings
                (LlmUrl "http://example.test/v1/chat/completions")
                (ModelName "file-model")
                (SystemPrompt "file rules")
                Nothing
                ModelShellDenied
            )
        modelExists <- doesFileExist modelPath
        promptExists <- doesFileExist promptPath
        assertBool "the model file should be removed" (not modelExists)
        assertBool "the system prompt file should be removed" (not promptExists)
    , testCase "parseLlmSettings parses each model shell policy" $ do
        let parsePolicy extra =
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
            settings policy =
              Right
                ( LlmSettings
                    (LlmUrl "http://example.test/v1/chat/completions")
                    (ModelName "m")
                    (SystemPrompt "")
                    Nothing
                    policy
                )
        parsePolicy [] @?= settings ModelShellDenied
        parsePolicy ["--model-shell", "deny"] @?= settings ModelShellDenied
        parsePolicy ["--model-shell", "ask"] @?= settings ModelShellAsk
        parsePolicy ["--model-shell", "allow"] @?= settings ModelShellAllowed
    , testCase "parseLlmSettings rejects an empty relay socket" $
        case
            parseLlmSettings
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
    , testCase "parseLlmSettings rejects empty URLs and model names" $ do
        case parseLlmSettings ["--llama-url", "", "--model", "m", "--system-prompt", ""] of
          Left message -> assertBool "empty url should be named" ("--llama-url" `isInfixOf` message)
          Right _ -> assertFailure "an empty url should fail"
        case parseLlmSettings ["--llama-url", "http://example.test", "--model", "", "--system-prompt", ""] of
          Left message -> assertBool "empty model should be named" ("--model" `isInfixOf` message)
          Right _ -> assertFailure "an empty model should fail"
    , testCase "parseLlmSettings reports missing options, missing values, and unknown arguments" $ do
        case parseLlmSettings [] of
          Left message -> do
            assertBool "missing url should be named" ("--llama-url" `isInfixOf` message)
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
            parseLlmSettings
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
        case parseLlmSettings ["--unknown", "value"] of
          Left message -> assertBool "unknown argument should be named" ("--unknown" `isInfixOf` message)
          Right _ -> assertFailure "unknown arguments should fail"
        case
            parseLlmSettings
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
    , testCase "chat request body has system and user messages" $
        case requestBodyFor ModelShellDenied ChatApi (ModelName "test-model") (SystemPrompt "system rules") emptyConversation "hello" of
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
              ModelShellDenied
              ChatApi
              (ModelName "test-model")
              (SystemPrompt "system rules")
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
        case requestBodyFor ModelShellDenied ChatApi (ModelName "test-model") (SystemPrompt "") emptyConversation "hello" of
          Object body ->
            case KeyMap.lookup "messages" body of
              Just (Array messages) -> Vector.length messages @?= 1
              other -> assertFailure ("missing messages: " ++ show other)
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "completion request body prepends the system prompt" $
        case requestBodyFor ModelShellDenied CompletionApi (ModelName "test-model") (SystemPrompt "system rules") emptyConversation "hello" of
          Object body -> do
            KeyMap.lookup "model" body @?= Just (String "test-model")
            KeyMap.lookup "prompt" body @?= Just (String "System: system rules\n\nUser: hello\n\nAssistant:")
            KeyMap.lookup "stream" body @?= Just (Bool True)
            KeyMap.lookup "messages" body @?= Nothing
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request body advertises the shell and job tools when allowed" $
        case requestBodyFor ModelShellAllowed ChatApi (ModelName "test-model") (SystemPrompt "") emptyConversation "hello" of
          Object body ->
            case KeyMap.lookup "tools" body of
              Just (Array tools) ->
                map toolName (Vector.toList tools)
                  @?= ["shell", "shell_background", "jobs", "job_output", "job_kill"]
              other -> assertFailure ("missing tools: " ++ show other)
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request body advertises the shell tool in ask mode" $
        case requestBodyFor ModelShellAsk ChatApi (ModelName "test-model") (SystemPrompt "") emptyConversation "hello" of
          Object body -> do
            assertBool "ask mode should advertise tools" (KeyMap.member "tools" body)
            KeyMap.lookup "tool_choice" body @?= Just (String "auto")
          other -> assertFailure ("unexpected request body: " ++ show other)
    , testCase "chat request body omits the shell tool when denied" $
        case requestBodyFor ModelShellDenied ChatApi (ModelName "test-model") (SystemPrompt "") emptyConversation "hello" of
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
        mergeToolCallDeltas [] [first, second]
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
      | "/v1/chat/completions" `isInfixOf` url = ChatApi
      | otherwise = CompletionApi

prop_parseSettings :: NonEmptyList Char -> NonEmptyList Char -> String -> Property
prop_parseSettings url model systemPrompt =
  parseLlmSettings
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
          ModelShellDenied
      )
  where
    urlText = getNonEmpty url
    modelText = getNonEmpty model

prop_parseSettingsWithSocket :: NonEmptyList Char -> NonEmptyList Char -> String -> NonEmptyList Char -> Property
prop_parseSettingsWithSocket url model systemPrompt socket =
  parseLlmSettings
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
          ModelShellDenied
      )
  where
    urlText = getNonEmpty url
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

prop_requestBody :: String -> String -> String -> Property
prop_requestBody model systemPrompt text =
  forAll (elements [ChatApi, CompletionApi]) $ \api ->
    counterexample (show (api, model, systemPrompt, text)) $
      case Aeson.decode (Aeson.encode (requestBodyFor ModelShellAllowed api (ModelName model) (SystemPrompt systemPrompt) emptyConversation text)) of
        Just (Object body) ->
          KeyMap.lookup "model" body === Just (String (Text.pack model))
            .&&. KeyMap.lookup "stream" body === Just (Bool True)
        _ -> counterexample "request body is not an object" False

toolName :: Value -> Text.Text
toolName (Object tool) =
  case KeyMap.lookup "function" tool of
    Just (Object functionObject) ->
      case KeyMap.lookup "name" functionObject of
        Just (String name) -> name
        _ -> ""
    _ -> ""
toolName _ = ""
