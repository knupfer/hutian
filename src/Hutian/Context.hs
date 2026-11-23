-- | Which agent turn is producing output, and where it sits in the agent path.
-- Kept in a small module so policy and verbosity code does not depend on the
-- ANSI rendering layer.
module Hutian.Context
  ( TurnContext (..)
  , isSubagentContext
  , contextPath
  , subagentContext
  , pathPrefix
  , pathLabel
  ) where

import Data.List (intercalate)

-- | A subagent carries its full path, outermost first. The main agent has an
-- empty path. Prompts use the path as a dotted prefix, for example
-- @foo.bar.think@.
data TurnContext
  = MainContext
  | SubagentContext [String]
  deriving stock (Eq, Show)

isSubagentContext :: TurnContext -> Bool
isSubagentContext MainContext = False
isSubagentContext (SubagentContext _) = True

contextPath :: TurnContext -> [String]
contextPath MainContext = []
contextPath (SubagentContext path) = path

-- | Extend a context with one more subagent name.
subagentContext :: TurnContext -> String -> TurnContext
subagentContext parent name = SubagentContext (contextPath parent ++ [name])

-- | The prefix for a prompt run in this context. Empty for the main agent.
pathPrefix :: TurnContext -> String
pathPrefix context =
  case contextPath context of
    [] -> ""
    path -> intercalate "." path ++ "."

-- | The path without a trailing dot, used for the subagent announcement.
pathLabel :: TurnContext -> String
pathLabel = intercalate "." . contextPath
