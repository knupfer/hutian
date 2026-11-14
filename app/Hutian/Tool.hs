-- | Pure turn of a model tool call into an action. The terminal loop in
-- "Main" only performs the IO that the plan asks for, so policy decisions stay
-- testable without a terminal or a shell.
module Hutian.Tool
  ( ToolPlan (..)
  , planTool
  ) where

import Data.Char (isSpace)
import Hutian.Conversation (ToolCall)
import Hutian.Llm.Protocol (toolCommand)
import Hutian.Policy (ModelShell (..))

-- | What the caller should do with one requested tool call.
data ToolPlan
  = -- | Run the command without asking.
    PlanRun String
  | -- | Show the command and ask the user before running it.
    PlanApprove String
  | -- | Send the message back to the model without running anything.
    PlanRefuse String
  deriving stock (Eq, Show)

-- | Resolve the policy and the call into an action. Unknown tools, malformed
-- arguments, empty commands, and a denied policy all become a refusal message
-- that is fed back to the model instead of reaching the shell.
planTool :: ModelShell -> ToolCall -> ToolPlan
planTool policy call =
  case toolCommand call of
    Left message -> PlanRefuse ("tool error: " ++ message)
    Right command
      | all isSpace command -> PlanRefuse "tool error: the shell command is empty"
      | otherwise ->
          case policy of
            ModelShellDenied -> PlanRefuse "tool error: the model shell tool is disabled"
            ModelShellAllowed -> PlanRun command
            ModelShellAsk -> PlanApprove command
