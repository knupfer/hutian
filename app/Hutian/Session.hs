-- | The interactive session loop. Prompting, history persistence, interrupt
-- clearing, and input execution are injected effects, so the sequencing can be
-- tested without a terminal, a model, or a job runtime.
module Hutian.Session
  ( Session (..)
  , SessionEffects (..)
  , runSession
  , rememberInput
  ) where

import Hutian.Conversation (Conversation)
import Hutian.Editor (InputMode, PromptBuffers)
import Hutian.History
  ( History
  , HistoryTarget
  , inputDestination
  , recordHistory
  )
import Hutian.History.Store
  ( HistoryFile
  , appendHistoryFile
  )

-- | Everything the loop carries between inputs. Settings and runtime handles
-- stay with the caller and are captured by 'SessionEffects'.
data Session = Session
  { sessionMode :: InputMode
  , sessionBuffers :: PromptBuffers
  , sessionHistory :: History
  , sessionConversation :: Conversation
  }

-- | The terminal and model work one loop iteration performs.
data SessionEffects = SessionEffects
  { -- | Read one input, returning the mode and buffers for the next prompt.
    sessionPrompt :: InputMode -> History -> PromptBuffers -> IO (InputMode, Maybe String, PromptBuffers)
  , -- | Persist and record a submitted input.
    sessionRememberInput :: InputMode -> String -> History -> IO History
  , -- | Clear a previous interrupt before running the input.
    sessionClearInterrupt :: IO ()
  , -- | Run a submitted input and return the updated conversation.
    sessionRunInput :: InputMode -> String -> Conversation -> IO Conversation
  }

-- | Prompt until input ends. Each submitted input is remembered, clears the
-- interrupt, runs, and leaves its conversation for the next turn.
runSession :: SessionEffects -> Session -> IO ()
runSession effects session = do
  (nextMode, input, nextBuffers) <-
    sessionPrompt effects
      (sessionMode session)
      (sessionHistory session)
      (sessionBuffers session)
  let prompted =
        session
          { sessionMode = nextMode
          , sessionBuffers = nextBuffers
          }
  case input of
    Nothing -> pure ()
    Just text -> do
      history' <- sessionRememberInput effects nextMode text (sessionHistory session)
      sessionClearInterrupt effects
      conversation <-
        sessionRunInput effects nextMode text (sessionConversation session)
      runSession
        effects
        prompted
          { sessionHistory = history'
          , sessionConversation = conversation
          }

-- | Append a non-blank input to the file for its mode and record it in memory.
-- Storage failures are ignored by "Hutian.History.Store".
rememberInput
  :: (HistoryTarget -> HistoryFile)
  -> InputMode
  -> String
  -> History
  -> IO History
rememberInput historyFile mode text history =
  case inputDestination mode text of
    Nothing -> pure history
    Just target -> do
      appendHistoryFile (historyFile target) text
      pure (recordHistory target text history)
