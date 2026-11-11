-- | Pure prompt interaction: key handling, history, mode switching, and
-- completion acceptance. Terminal IO lives in "Hutian.Terminal".
module Hutian.Prompt
  ( PromptOutcome (..)
  , PromptState
  , PromptStep (..)
  , acceptCompletion
  , initialPromptState
  , promptBuffers
  , promptMode
  , stepPrompt
  ) where

import Hutian.Editor
  ( Prompt
  , PromptBuffers
  , PromptAction (..)
  , PromptMode (..)
  , activePrompt
  , applyKey
  , clearPrompt
  , clearSuggestions
  , cursorToEnd
  , promptAction
  , promptLines
  , promptSuggestions
  , storePrompt
  , toggleMode
  )
import Hutian.History
  ( History
  , HistoryState
  , historyDown
  , historyForMode
  , historyUp
  , initialHistoryState
  )
import Hutian.Key (Key (..))

-- | What the caller should do after a key press.
data PromptStep
  = StepContinue PromptState
  | StepRedraw PromptState
  | StepMoveCursor Prompt PromptState
  | StepComplete PromptState
  | StepFinish PromptOutcome Prompt PromptState

-- | How the prompt ended.
data PromptOutcome
  = PromptSubmitted
  | PromptExited
  deriving stock (Eq, Show)

-- | Everything the prompt carries between key presses. The cursor position is
-- kept by the driver because it is derived from terminal width.
{-@ data PromptState = PromptState
      { promptMode :: PromptMode
      , promptHistoryState :: HistoryState
      , promptBuffers :: PromptBuffers
      } @-}
data PromptState = PromptState
  { promptMode :: PromptMode
  , promptHistoryState :: HistoryState
  , promptBuffers :: PromptBuffers
  }

{-@ initialPromptState :: mode:PromptMode -> PromptBuffers ->
      {v:PromptState | promptMode v == mode} @-}
initialPromptState :: PromptMode -> PromptBuffers -> PromptState
initialPromptState mode buffers =
  PromptState
    { promptMode = mode
    , promptHistoryState = initialHistoryState
    , promptBuffers = buffers
    }

-- | Apply one key to the prompt state. The result tells the driver whether to
-- redraw, move the cursor, ask for filesystem completion, or finish.
stepPrompt :: History -> Key -> PromptState -> PromptStep
stepPrompt history key state =
  case promptAction mode current key of
    PromptSubmit ->
      StepFinish
        PromptSubmitted
        current
        state
          { promptHistoryState = initialHistoryState
          , promptBuffers = clearPrompt mode (promptBuffers state)
          }
    PromptExit -> StepFinish PromptExited current state
    PromptContinue ->
      StepRedraw
        state
          { promptHistoryState = initialHistoryState
          , promptBuffers = storePrompt mode (applyKey KeyEnter (cursorToEnd current)) (promptBuffers state)
          }
    PromptClear ->
      StepRedraw
        state
          { promptHistoryState = initialHistoryState
          , promptBuffers = clearPrompt mode (promptBuffers state)
          }
    PromptEdit key' ->
      let updated = applyKey key' current
          next =
            state
              { promptHistoryState = initialHistoryState
              , promptBuffers = storePrompt mode updated (promptBuffers state)
              }
       in if sameBuffer current updated
            then StepMoveCursor updated next
            else StepRedraw next
    PromptHistoryUp -> navigate historyUp state
    PromptHistoryDown -> navigate historyDown state
    PromptSwitchMode ->
      StepRedraw
        state
          { promptMode = toggleMode mode
          , promptHistoryState = initialHistoryState
          , promptBuffers = storePrompt mode (clearSuggestions current) (promptBuffers state)
          }
    PromptComplete -> StepComplete state
  where
    mode = promptMode state
    current = activePrompt mode (promptBuffers state)
    entries = historyForMode mode history

    navigate step promptState =
      case step entries (promptHistoryState promptState) (activePrompt mode (promptBuffers promptState)) of
        Nothing -> StepContinue promptState
        Just (historyState', updated) ->
          StepRedraw
            promptState
              { promptHistoryState = historyState'
              , promptBuffers = storePrompt mode updated (promptBuffers promptState)
              }

-- | Store a completed prompt and reset history navigation.
{-@ acceptCompletion :: completed:Prompt -> state:PromptState ->
      {v:PromptState | promptMode v == promptMode state} @-}
acceptCompletion :: Prompt -> PromptState -> PromptState
acceptCompletion completed state =
  state
    { promptHistoryState = initialHistoryState
    , promptBuffers = storePrompt (promptMode state) completed (promptBuffers state)
    }

sameBuffer :: Prompt -> Prompt -> Bool
sameBuffer a b =
  promptLines a == promptLines b
    && promptSuggestions a == promptSuggestions b
