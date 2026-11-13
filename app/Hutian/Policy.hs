-- | Model shell-tool authorization policy. User term mode is always available,
-- so this only controls what the LLM is told it may call and whether requested
-- tool calls are executed.
module Hutian.Policy
  ( ShellAccess (..)
  , shellAllowed
  ) where

-- | Whether the model may call the shell tool. This gates tool advertisement
-- and tool-call execution only; it does not affect the user's term mode.
data ShellAccess
  = ShellAllowed
  | ShellDenied
  deriving stock (Eq, Show)

shellAllowed :: ShellAccess -> Bool
shellAllowed ShellAllowed = True
shellAllowed ShellDenied = False
