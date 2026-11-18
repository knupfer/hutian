-- | Pure completion selection shared by command and path completion.
module Hutian.Completion
  ( Completion (..)
  , CompletionKind (..)
  , chooseCompletion
  , longestCommonPrefix
  ) where

import Hutian.Paths (hasTrailingSlash)

-- Explicit recursion lets LiquidHaskell prove the common-prefix length bound.
{- HLINT ignore "Use foldl" -}

{-@ data Completion = Completion
      { completionMatches :: [String]
      , completionReplacement :: String
      } @-}
data Completion = Completion
  { completionMatches :: [String]
  , completionReplacement :: String
  }
  deriving stock (Eq, Show)

-- | How a unique match should be followed by a space.
data CompletionKind
  = CommandCompletion
  | PathCompletion
  deriving stock (Eq, Show)

-- | Choose the replacement text for a completion attempt.
--
-- The kind decides whether a unique match should be followed by a space.
-- Callers pass the current word, the text after the cursor, and the matches.
{-@ chooseCompletion :: kind:CompletionKind -> word:String -> afterCursor:String -> matches:[String]
      -> {v:Completion | completionMatches v == matches
                       && (len matches == 0 => completionReplacement v == word)} @-}
chooseCompletion :: CompletionKind -> String -> String -> [String] -> Completion
chooseCompletion kind word afterCursor matches =
  case matches of
    [only] -> Completion matches (only ++ suffix only)
    _ -> Completion matches (extend word)
  where
    common = longestCommonPrefix matches
    suffix only
      | null afterCursor && appendSpace only = " "
      | otherwise = ""
    appendSpace only =
      case kind of
        CommandCompletion -> True
        PathCompletion -> not (hasTrailingSlash only)
    extend current
      | length common > length current = common
      | otherwise = current

{-@ longestCommonPrefix :: xs:[String]
      -> {v:String | (len xs == 0 => len v == 0)
                   && (len xs > 0 => len v <= len (head xs))} @-}
longestCommonPrefix :: [String] -> String
longestCommonPrefix [] = ""
longestCommonPrefix (first : rest) = longestCommonPrefixFrom first rest

{-@ longestCommonPrefixFrom :: first:String -> rest:[String] -> {v:String | len v <= len first} @-}
longestCommonPrefixFrom :: String -> [String] -> String
longestCommonPrefixFrom first [] = first
longestCommonPrefixFrom first (next : rest) =
  longestCommonPrefixFrom (commonPrefix first next) rest

{-@ commonPrefix :: a:String -> b:String -> {v:String | len v <= len a && len v <= len b} @-}
commonPrefix :: String -> String -> String
commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))
