-- | The model/tool exchange loop. The two effects it needs are injected so the
-- sequencing can be tested without a model, a terminal, or a job runtime.
module Hutian.Llm.Loop
  ( LlmLoop (..)
  , runToolLoop
  , runToolLoopWithResult
  ) where

import Hutian.Conversation
  ( Conversation
  , ToolCall
  , abortToolCalls
  , appendResult
  )
import Hutian.Interrupt (InterruptFlag, isInterrupted)
import Hutian.Llm.Stream (LlmResult (..))
import Hutian.Tool (ToolRun (..))

-- | One iteration's effects.
data LlmLoop = LlmLoop
  { -- | Run the requested tool calls. The status reports that the turn aborted,
    -- so no further calls or continuations may run.
    loopRunTools :: Conversation -> [ToolCall] -> IO (ToolRun, Conversation)
  , -- | Ask the model to continue after the tool results.
    loopContinue :: Conversation -> IO LlmResult
  }

-- | Keep executing tool calls and feeding the results back until the model
-- answers without requesting more. An interrupt before execution records
-- aborted results instead of running anything, because the call arguments may
-- be truncated.
runToolLoop :: LlmLoop -> InterruptFlag -> Conversation -> LlmResult -> IO Conversation
runToolLoop loop interruptFlag conversation result =
  fst <$> runToolLoopWithResult loop interruptFlag conversation result

-- | 'runToolLoop' plus the result that ended the turn. A caller that needs the
-- final answer, such as a subagent returning only its conclusion, uses this.
runToolLoopWithResult :: LlmLoop -> InterruptFlag -> Conversation -> LlmResult -> IO (Conversation, LlmResult)
runToolLoopWithResult loop interruptFlag conversation result
  | null (llmToolCalls result) = pure (conversation, result)
  | otherwise = do
      interrupted <- isInterrupted interruptFlag
      if interrupted
        then pure (abortToolCalls conversation (llmToolCalls result), result)
        else do
          (status, conversation') <-
            loopRunTools loop conversation (llmToolCalls result)
          case status of
            ToolsAborted -> pure (conversation', result)
            ToolsCompleted -> do
              next <- loopContinue loop conversation'
              runToolLoopWithResult
                loop
                interruptFlag
                (appendResult Nothing (llmResponseText next) (llmToolCalls next) conversation')
                next
