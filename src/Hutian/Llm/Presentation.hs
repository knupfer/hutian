-- | Terminal presentation for streamed model output. The protocol reducer in
-- "Hutian.Llm.Stream" carries no rendering state; this module owns incremental
-- Markdown and the labels and colours around reasoning and answers.
module Hutian.Llm.Presentation
  ( StreamPresenter
  , initialStreamPresenter
  , presentStreamEvent
  , finishPresentation
  ) where

import qualified Data.Text as Text
import Hutian.Llm.Stream (StreamEvent (..))
import Hutian.Markdown
  ( MarkdownState
  , finishMarkdown
  , finishMarkdownWith
  , initialMarkdownState
  , renderMarkdown
  , renderMarkdownWith
  )
import Hutian.Render (boldTeal, dimTealForeground, grayForeground, reset)

-- | Rendering state for one streamed response. Reasoning and answer Markdown
-- are kept separately so the answer can flush the reasoning buffer first.
data StreamPresenter = StreamPresenter
  { presenterReasoning :: MarkdownState
  , presenterReasoningStarted :: Bool
  , presenterAnswer :: MarkdownState
  , presenterAnswerStarted :: Bool
  }

initialStreamPresenter :: StreamPresenter
initialStreamPresenter =
  StreamPresenter
    { presenterReasoning = initialMarkdownState
    , presenterReasoningStarted = False
    , presenterAnswer = initialMarkdownState
    , presenterAnswerStarted = False
    }

-- | Render one event into terminal text. Errors stay on the event so the
-- caller's diagnostics boundary decides where they go.
presentStreamEvent :: StreamEvent -> StreamPresenter -> (StreamPresenter, String)
presentStreamEvent event presenter =
  (contentPresenter, reasoningOutput ++ contentOutput)
  where
    (reasoningPresenter, reasoningOutput) =
      appendReasoning (eventReasoning event) presenter
    (contentPresenter, contentOutput) =
      appendContent (eventContent event) reasoningPresenter

appendReasoning :: Maybe Text.Text -> StreamPresenter -> (StreamPresenter, String)
appendReasoning Nothing presenter = (presenter, "")
appendReasoning (Just text) presenter
  | Text.null text = (presenter, "")
  | otherwise = (presenter', header ++ output)
  where
    header
      | presenterReasoningStarted presenter = ""
      | otherwise = dimTealForeground ++ "think:" ++ reset ++ "\n"
    (markdown', output) =
      renderMarkdownWith thinkingGrey (presenterReasoning presenter) text
    presenter' =
      presenter
        { presenterReasoning = markdown'
        , presenterReasoningStarted = True
        }

appendContent :: Maybe Text.Text -> StreamPresenter -> (StreamPresenter, String)
appendContent Nothing presenter = (presenter, "")
appendContent (Just text) presenter
  | Text.null text = (presenter, "")
  | otherwise = (presenter', flushed ++ header ++ output)
  where
    (startingAnswer, flushed) = flushReasoning presenter
    header
      | presenterAnswerStarted startingAnswer = ""
      | otherwise = boldTeal ++ "answer:" ++ reset ++ "\n"
    (markdown', output) = renderMarkdown (presenterAnswer startingAnswer) text
    presenter' =
      startingAnswer
        { presenterAnswer = markdown'
        , presenterAnswerStarted = True
        }

-- | Flush buffered reasoning text when the answer begins or the stream ends.
flushReasoning :: StreamPresenter -> (StreamPresenter, String)
flushReasoning presenter
  | not (presenterReasoningStarted presenter) = (presenter, "")
  | otherwise = (presenter {presenterReasoning = initialMarkdownState}, output)
  where
    trailing = finishMarkdownWith thinkingGrey (presenterReasoning presenter)
    output = (if null trailing then "" else trailing ++ "\n") ++ reset

-- | Flush the final partial answer and any open code fence.
finishPresentation :: StreamPresenter -> (StreamPresenter, String)
finishPresentation presenter =
  (stateAfterReasoning, reasoningOutput ++ answerOutput)
  where
    (stateAfterReasoning, reasoningOutput) = flushReasoning presenter
    trailing = finishMarkdown (presenterAnswer stateAfterReasoning)
    answerOutput = trailing ++ if null trailing then "" else "\n"

-- | Thinking text uses the same grey as command output.
thinkingGrey :: String
thinkingGrey = grayForeground
