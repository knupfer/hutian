-- | The model/tool exchange loop. The two effects it needs are injected so the
-- sequencing can be tested without a model, a terminal, or a job runtime.
module Hutian.Llm.Loop
  ( LlmLoop (..)
  , runToolLoop
  ) where

import Hutian.Conversation
  ( Conversation
  , ToolCall
  , abortToolCalls
  , appendResult
  )
import Hutian.Interrupt (InterruptFlag, isInterrupted)
import Hutian.Llm.Stream (LlmResult (..))

-- | One iteration's effects.
data LlmLoop = LlmLoop
  { -- | Run the requested tool calls. The flag reports that the turn aborted,
    -- so no further calls or continuations may run.
    loopRunTools :: Conversation -> [ToolCall] -> IO (Bool, Conversation)
  , -- | Ask the model to continue after the tool results.
    loopContinue :: Conversation -> IO LlmResult
  }

-- | Keep executing tool calls and feeding the results back until the model
-- answers without requesting more. An interrupt before execution records
-- aborted results instead of running anything, because the call arguments may
-- be truncated.
runToolLoop :: LlmLoop -> InterruptFlag -> Conversation -> LlmResult -> IO Conversation
runToolLoop loop interruptFlag conversation result
  | null (llmToolCalls result) = pure conversation
  | otherwise = do
      interrupted <- isInterrupted interruptFlag
      if interrupted
        then pure (abortToolCalls conversation (llmToolCalls result))
        else do
          (aborted, conversation') <-
            loopRunTools loop conversation (llmToolCalls result)
          if aborted
            then pure conversation'
            else do
              next <- loopContinue loop conversation'
              runToolLoop
                loop
                interruptFlag
                (appendResult Nothing (llmResponseText next) (llmToolCalls next) conversation')
                next
