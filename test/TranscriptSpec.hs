{-# LANGUAGE OverloadedStrings #-}

module TranscriptSpec (tests) where

import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as Text
import Hutian.Context (TurnContext (MainContext))
import Hutian.Conversation
  ( ConversationEntry (..)
  , ToolCall (..)
  )
import Hutian.Editor (InputMode (..))
import Hutian.Render (ToolApproval (..))
import Hutian.Transcript
  ( SessionInfo (..)
  , ToolOutcome (..)
  , ToolResult (..)
  , TranscriptEvent (..)
  , TranscriptPaths (..)
  , encodeTranscriptEvent
  , renderTranscriptEvent
  , shiftMarkdownHeadings
  , transcriptPaths
  )
import Hutian.Transcript.Recorder
  ( closeTranscript
  , openTranscript
  , recordTranscript
  )
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import TestSupport (withTemporaryDirectory)

tests :: TestTree
tests =
  testGroup
    "transcript"
    [ testCase "transcript paths pair the two files" $ do
        transcriptPaths "session.md"
          @?= TranscriptPaths "session.jsonl" "session.md"
        transcriptPaths "session.jsonl"
          @?= TranscriptPaths "session.jsonl" "session.md"
        transcriptPaths "session"
          @?= TranscriptPaths "session.jsonl" "session.md"
        transcriptPaths "sessions/"
          @?= TranscriptPaths "sessions/transcript.jsonl" "sessions/transcript.md"
    , testCase "prompt headings use the ask and shell labels and are not fenced" $ do
        renderTranscriptEvent "now" (TranscriptInput LlmInput "hello")
          @?= "\n# ask\nhello\n"
        renderTranscriptEvent "now" (TranscriptTerminalCommand "ls\n-l")
          @?= "\n# shell\nls\n-l\n"
    , testCase "agent prompts are level-one headings and mcp names are dotted" $ do
        renderTranscriptEvent "now" (TranscriptAgentPrompt MainContext "ask" "do it")
          @?= "\n# ask\ndo it\n"
        renderTranscriptEvent
          "now"
          ( TranscriptToolRequest
              MainContext
              "mcp.demo.search"
              (ToolCall "call-1" "mcp__demo__search" "{\"query\":\"x\"}")
          )
          @?= "\n# mcp.demo.search\n{\"query\":\"x\"}\n"
    , testCase "blocks are separated by one blank line" $ do
        let rendered =
              concat
                [ renderTranscriptEvent "now" (TranscriptInput LlmInput "hello")
                , renderTranscriptEvent "now" (TranscriptReasoning MainContext "reason")
                , renderTranscriptEvent "now" (TranscriptAssistant MainContext "answer" [])
                ]
        assertBool "no triple newline between blocks" (not ("\n\n\n" `isInfixOf` rendered))
    , testCase "headings inside response blocks move down one level" $ do
        shiftMarkdownHeadings 1 "# A\n## B\nbody\n"
          @?= "## A\n### B\nbody\n"
        shiftMarkdownHeadings 1 "###### six\n"
          @?= "###### six\n"
    , testCase "headings inside fenced code stay unchanged" $
        shiftMarkdownHeadings 1 "```\n# not a heading\n```\n"
          @?= "```\n# not a heading\n```\n"
    , testCase "session header contains only the model" $
        renderTranscriptEvent
          "now"
          (TranscriptSessionStarted (SessionInfo "model"))
          @?= "Hutian session — now\n\n- model: `model`\n\n---\n"
    , testCase "tool results are not fenced and their headings are demoted" $
        renderTranscriptEvent
          "now"
          ( TranscriptToolResult
              MainContext
              (ToolResult "call-1" "shell" ToolSucceeded "# out\n")
          )
          @?= "\n# shell result\n## out\n"
    , testCase "assistant markdown cannot collide with prompt headings" $ do
        let rendered =
              renderTranscriptEvent "now" (TranscriptAssistant MainContext "# Answer\n" [])
        assertBool "the block heading is level one" ("\n# answer\n" `isPrefixOf` rendered)
        assertBool "the answer heading moves to level two" ("\n## Answer" `isInfixOf` rendered)
        assertBool "no level-one answer heading remains" (not ("\n# Answer" `isInfixOf` rendered))
    , testCase "events encode with the schema version and type" $
        ( Aeson.decode (encodeTranscriptEvent "2026-11-26T10:00:00Z" (TranscriptInput LlmInput "hi"))
            :: Maybe Value
        )
          @?= Just
            ( object
                [ "schema" .= (1 :: Int)
                , "at" .= ("2026-11-26T10:00:00Z" :: String)
                , "type" .= ("input" :: String)
                , "mode" .= ("llm" :: String)
                , "text" .= ("hi" :: String)
                ]
            )
    , testCase "every event type encodes its envelope and type" $
        forM_
          [ (TranscriptSessionStarted (SessionInfo "model"), "session.start")
          , (TranscriptSessionEnded "exit", "session.end")
          , (TranscriptInput LlmInput "hi", "input")
          , (TranscriptTerminalCommand "ls", "term.command")
          , (TranscriptReasoning MainContext "why", "reasoning")
          , (TranscriptAssistant MainContext "answer" [], "assistant")
          , (TranscriptAgentPrompt MainContext "ask" "task", "agent.prompt")
          , ( TranscriptToolRequest
                MainContext
                "shell"
                (ToolCall "call-1" "shell" "{}")
            , "tool.request"
            )
          , (TranscriptToolDecision MainContext "call-1" ToolApproved, "tool.decision")
          , ( TranscriptToolResult
                MainContext
                (ToolResult "call-1" "shell" ToolSucceeded "ok")
            , "tool.result"
            )
          , (TranscriptDiagnostic "warning", "diagnostic")
          , (TranscriptInterrupted MainContext, "interrupted")
          , (TranscriptConversationSnapshot [UserEntry "hi"], "conversation.snapshot")
          ]
          $ \(event, expectedType) ->
            case Aeson.decode (encodeTranscriptEvent "now" event) :: Maybe Value of
              Just (Aeson.Object fields) -> do
                KeyMap.lookup "type" fields
                  @?= Just (Aeson.String (Text.pack expectedType))
                KeyMap.lookup "schema" fields @?= Just (Aeson.toJSON (1 :: Int))
                KeyMap.lookup "at" fields @?= Just (Aeson.String "now")
              _ -> assertFailure ("event did not encode as an object: " ++ expectedType)
    , testCase "conversation snapshots preserve every entry kind" $ do
        let conversation =
              [ UserEntry "hi"
              , AssistantEntry "hello"
              , TerminalEntry "ls"
              , AssistantToolCallEntry "checking" [ToolCall "call-1" "shell" "{}"]
              , ToolResultEntry "call-1" "ok"
              ]
            snapshot = TranscriptConversationSnapshot conversation
        (Aeson.decode (encodeTranscriptEvent "now" snapshot) :: Maybe Value)
          @?= Just
            ( object
                [ "schema" .= (1 :: Int)
                , "at" .= ("now" :: String)
                , "type" .= ("conversation.snapshot" :: String)
                , "conversation"
                    .= [ object ["kind" .= ("user" :: String), "text" .= ("hi" :: String)]
                       , object ["kind" .= ("assistant" :: String), "text" .= ("hello" :: String)]
                       , object ["kind" .= ("terminal" :: String), "command" .= ("ls" :: String)]
                       , object
                           [ "kind" .= ("assistant_tool_calls" :: String)
                           , "text" .= ("checking" :: String)
                           , "toolCalls"
                               .= [ object
                                      [ "id" .= ("call-1" :: String)
                                      , "name" .= ("shell" :: String)
                                      , "arguments" .= ("{}" :: String)
                                      ]
                                  ]
                           ]
                       , object
                           [ "kind" .= ("tool_result" :: String)
                           , "callId" .= ("call-1" :: String)
                           , "text" .= ("ok" :: String)
                           ]
                       ]
                ]
            )
        renderTranscriptEvent "now" snapshot @?= ""
    , testCase "streamed reasoning is one markdown block and both files are written" $
        withTemporaryDirectory "hutian-transcript" $ \directory -> do
          let base = directory </> "session"
          transcript <- openTranscript base
          recordTranscript transcript (TranscriptInput LlmInput "hello")
          recordTranscript transcript (TranscriptReasoning MainContext "think ")
          recordTranscript transcript (TranscriptReasoning MainContext "more")
          recordTranscript transcript (TranscriptAssistant MainContext "answer" [])
          recordTranscript
            transcript
            (TranscriptConversationSnapshot [UserEntry "hello", AssistantEntry "answer"])
          closeTranscript transcript
          jsonLines <- lines <$> readFile (base ++ ".jsonl")
          markdown <- readFile (base ++ ".md")
          length jsonLines @?= 5
          assertBool "the final snapshot is written" $
            any ("\"type\":\"conversation.snapshot\"" `isInfixOf`) jsonLines
          countInfix "\"type\":\"reasoning\"" (unlines jsonLines) @?= 2
          assertBool "the first reasoning chunk is durable" $
            any ("think " `isInfixOf`) jsonLines
          assertBool "the second reasoning chunk is durable" $
            any ("more" `isInfixOf`) jsonLines
          assertBool "the prompt heading is present" ("# ask" `isInfixOf` markdown)
          countInfix "# thinking" markdown @?= 1
          assertBool "the answer is present" ("answer" `isInfixOf` markdown)
    ]

countInfix :: String -> String -> Int
countInfix needle = go
  where
    go [] = 0
    go haystack@(_ : rest)
      | needle `isPrefixOf` haystack = 1 + go (drop (length needle) haystack)
      | otherwise = go rest
