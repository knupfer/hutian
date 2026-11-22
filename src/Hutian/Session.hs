-- | The interactive session loop. Prompting, interrupt clearing, and input
-- execution are injected effects, so the sequencing can be tested without a
-- terminal, a model, or a job runtime.
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
  , inputDestination
  , recordHistory
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
      let history' = rememberInput nextMode text (sessionHistory session)
      sessionClearInterrupt effects
      conversation <-
        sessionRunInput effects nextMode text (sessionConversation session)
      runSession
        effects
        prompted
          { sessionHistory = history'
          , sessionConversation = conversation
          }

-- | Record a non-blank input in the history for its mode.
rememberInput :: InputMode -> String -> History -> History
rememberInput mode text history =
  case inputDestination mode text of
    Nothing -> history
    Just target -> recordHistory target text history
