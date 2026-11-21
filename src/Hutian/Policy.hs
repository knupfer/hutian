{-# LANGUAGE OverloadedStrings #-}

-- | Authorization policy for every model tool. User term mode is always
-- available, so this only controls what the LLM is told it may call and how a
-- requested call is handled.
module Hutian.Policy
  ( ToolPolicy (..)
  , toolPolicyAdvertised
  , toolPolicyArgument
  , toolPolicyFromArgument
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (String)
  , withText
  )
import qualified Data.Text as Text

-- | A three-way policy shared by the shell, web, and subagent tools.
-- 'ToolAsk' still advertises the tool, but the call needs interactive
-- approval before it runs.
data ToolPolicy
  = ToolDeny
  | ToolAsk
  | ToolAllow
  deriving stock (Eq, Show)

-- | Whether the tool appears in LLM requests. Ask mode must advertise the
-- tool so the model can request it and the user can approve.
toolPolicyAdvertised :: ToolPolicy -> Bool
toolPolicyAdvertised ToolDeny = False
toolPolicyAdvertised ToolAsk = True
toolPolicyAdvertised ToolAllow = True

-- | The stable command-line and JSON spelling of a policy.
{-@ toolPolicyArgument :: ToolPolicy -> {v:String | len v >= 3} @-}
toolPolicyArgument :: ToolPolicy -> String
toolPolicyArgument ToolDeny = "deny"
toolPolicyArgument ToolAsk = "ask"
toolPolicyArgument ToolAllow = "allow"

-- | Parse the launcher-provided policy.
toolPolicyFromArgument :: String -> Maybe ToolPolicy
toolPolicyFromArgument "deny" = Just ToolDeny
toolPolicyFromArgument "ask" = Just ToolAsk
toolPolicyFromArgument "allow" = Just ToolAllow
toolPolicyFromArgument _ = Nothing

instance ToJSON ToolPolicy where
  toJSON = String . Text.pack . toolPolicyArgument

instance FromJSON ToolPolicy where
  parseJSON = withText "tool policy" $ \text ->
    case toolPolicyFromArgument (Text.unpack text) of
      Just policy -> pure policy
      Nothing ->
        fail
          ( "invalid tool policy: "
              ++ Text.unpack text
              ++ " (expected deny, ask, or allow)"
          )
