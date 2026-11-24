-- | Terminal presentation for streamed model output. The protocol reducer in
-- "Hutian.Llm.Stream" carries no rendering state; this module owns incremental
-- Markdown and the labels and colours around reasoning and answers.
module Hutian.Llm.Presentation
  ( StreamPresenter
  , initialStreamPresenter
  , initialStreamPresenterIn
  , presentStreamEvent
  , finishPresentation
  ) where

import qualified Data.Text as Text
import Hutian.Context (TurnContext (MainContext))
import Hutian.Llm.Stream (StreamEvent (..))
import Hutian.Markdown
  ( MarkdownFlavor (..)
  , MarkdownState
  , finishMarkdown
  , finishMarkdownWith
  , initialMarkdownState
  , renderMarkdown
  , renderMarkdownWith
  )
import Hutian.Render
  ( answerLabelText
  , grayForeground
  , reasoningLabelText
  , reset
  )
import Hutian.Verbosity
  ( AnswerStyle (..)
  , RenderMode (..)
  , Verbosity (..)
  , Visibility (..)
  , renderMode
  )

-- | Rendering state for one streamed response. Reasoning and answer Markdown
-- are kept separately so the answer can flush the reasoning buffer first.
data StreamPresenter = StreamPresenter
  { presenterRenderMode :: RenderMode
  , presenterContext :: TurnContext
  , presenterReasoning :: MarkdownState
  , presenterReasoningStarted :: Bool
  , presenterAnswer :: MarkdownState
  , presenterAnswerStarted :: Bool
  }

initialStreamPresenter :: StreamPresenter
initialStreamPresenter = initialStreamPresenterIn VerbosityHigh MainContext

initialStreamPresenterIn :: Verbosity -> TurnContext -> StreamPresenter
initialStreamPresenterIn verbosity context =
  StreamPresenter
    { presenterRenderMode = renderMode verbosity context
    , presenterContext = context
    , presenterReasoning = initialMarkdownState
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
  | otherwise =
      case modeReasoning mode of
        LabelOnly ->
          (presenter {presenterReasoningStarted = True}, header)
        Full -> (presenter', header ++ output)
  where
    mode = presenterRenderMode presenter
    header
      | presenterReasoningStarted presenter = ""
      | otherwise = reasoningLabelText (presenterContext presenter) ++ "\n"
    (markdown', output) =
      renderMarkdownWith (DimMarkdown thinkingGrey) (presenterReasoning presenter) text
    presenter' =
      presenter
        { presenterReasoning = markdown'
        , presenterReasoningStarted = True
        }

appendContent :: Maybe Text.Text -> StreamPresenter -> (StreamPresenter, String)
appendContent Nothing presenter = (presenter, "")
appendContent (Just text) presenter
  | Text.null text = (presenter, "")
  | otherwise =
      case modeAnswer mode of
        LabelOnly ->
          (startingAnswer {presenterAnswerStarted = True}, flushed ++ header)
        Full -> (presenter', flushed ++ header ++ output)
  where
    mode = presenterRenderMode presenter
    (startingAnswer, flushed) = flushReasoning presenter
    header
      | presenterAnswerStarted startingAnswer = ""
      | otherwise = answerLabelText (presenterContext startingAnswer) ++ "\n"
    (markdown', output) =
      renderAnswer
        mode
        (presenterAnswer startingAnswer)
        text
    presenter' =
      startingAnswer
        { presenterAnswer = markdown'
        , presenterAnswerStarted = True
        }

-- | Flush buffered reasoning text when the answer begins or the stream ends.
flushReasoning :: StreamPresenter -> (StreamPresenter, String)
flushReasoning presenter
  | not (presenterReasoningStarted presenter) = (presenter, "")
  | otherwise =
      case modeReasoning mode of
        LabelOnly -> (resetReasoning presenter, "")
        Full -> (resetReasoning presenter, output)
  where
    mode = presenterRenderMode presenter
    trailing = finishMarkdownWith (DimMarkdown thinkingGrey) (presenterReasoning presenter)
    output = (if null trailing then "" else trailing ++ "\n") ++ reset

resetReasoning :: StreamPresenter -> StreamPresenter
resetReasoning presenter =
  presenter
    { presenterReasoning = initialMarkdownState
    , presenterReasoningStarted = False
    }

-- | Flush the final partial answer and any open code fence. A subagent's
-- answer keeps the same dim base style as a thinking block.
finishPresentation :: StreamPresenter -> (StreamPresenter, String)
finishPresentation presenter =
  (stateAfterReasoning, reasoningOutput ++ answerOutput)
  where
    (stateAfterReasoning, reasoningOutput) = flushReasoning presenter
    trailing =
      finishAnswer
        (presenterRenderMode stateAfterReasoning)
        (presenterAnswer stateAfterReasoning)
    answerOutput = trailing ++ if null trailing then "" else "\n"

renderAnswer :: RenderMode -> MarkdownState -> Text.Text -> (MarkdownState, String)
renderAnswer mode
  | modeAnswerStyle mode == AnswerDim = renderMarkdownWith (DimMarkdown thinkingGrey)
  | otherwise = renderMarkdown

finishAnswer :: RenderMode -> MarkdownState -> String
finishAnswer mode
  | modeAnswerStyle mode == AnswerDim = finishMarkdownWith (DimMarkdown thinkingGrey)
  | otherwise = finishMarkdown

-- | Thinking text uses the same grey as command output.
thinkingGrey :: String
thinkingGrey = grayForeground
