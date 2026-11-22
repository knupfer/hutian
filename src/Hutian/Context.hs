-- | Which agent turn is producing output. Kept in a small module so policy and
-- verbosity code does not depend on the ANSI rendering layer.
module Hutian.Context
  ( TurnContext (..)
  ) where

-- | Everything a subagent prints uses the subagent blue; the main turn uses the
-- main palette.
data TurnContext
  = MainContext
  | SubagentContext
  deriving stock (Eq, Show)
