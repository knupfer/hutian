-- | Pure completion selection shared by command and path completion.
module Hutian.Completion
  ( Completion (..)
  , chooseCompletion
  , longestCommonPrefix
  ) where

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

-- | Choose the replacement text for a completion attempt.
--
-- The predicate decides whether a unique match should be followed by a space.
-- Callers pass the current word, the text after the cursor, and the matches.
{-@ chooseCompletion :: appendSpace:(String -> Bool) -> word:String -> afterCursor:String -> matches:[String]
      -> {v:Completion | completionMatches v == matches
                       && (len matches == 0 => completionReplacement v == word)} @-}
chooseCompletion :: (String -> Bool) -> String -> String -> [String] -> Completion
chooseCompletion appendSpace word afterCursor matches =
  case matches of
    [only] -> Completion matches (only ++ suffix only)
    _ -> Completion matches (extend word)
  where
    common = longestCommonPrefix matches
    suffix only
      | null afterCursor && appendSpace only = " "
      | otherwise = ""
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
