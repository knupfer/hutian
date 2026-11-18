-- | Model shell-tool authorization policy. User term mode is always available,
-- so this only controls what the LLM is told it may call and how requested tool
-- calls are handled.
module Hutian.Policy
  ( ModelShell (..)
  , modelShellAdvertised
  , modelShellArgument
  , modelShellFromArgument
  ) where

-- | How the model's shell tool is handled. 'ModelShellAsk' still advertises the
-- tool, but every call needs interactive approval before it runs.
data ModelShell
  = ModelShellDenied
  | ModelShellAsk
  | ModelShellAllowed
  deriving stock (Eq, Show)

-- | Whether the shell tool appears in LLM requests. Ask mode must advertise the
-- tool so the model can request a command and the user can approve it.
modelShellAdvertised :: ModelShell -> Bool
modelShellAdvertised ModelShellDenied = False
modelShellAdvertised ModelShellAsk = True
modelShellAdvertised ModelShellAllowed = True

-- | The stable command-line and Nix spelling of a policy.
{-@ modelShellArgument :: ModelShell -> {v:String | len v >= 3} @-}
modelShellArgument :: ModelShell -> String
modelShellArgument ModelShellDenied = "deny"
modelShellArgument ModelShellAsk = "ask"
modelShellArgument ModelShellAllowed = "allow"

-- | Parse the launcher-provided policy.
modelShellFromArgument :: String -> Maybe ModelShell
modelShellFromArgument "deny" = Just ModelShellDenied
modelShellFromArgument "ask" = Just ModelShellAsk
modelShellFromArgument "allow" = Just ModelShellAllowed
modelShellFromArgument _ = Nothing
