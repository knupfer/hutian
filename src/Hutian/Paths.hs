-- | Pure path completion helpers.
module Hutian.Paths
  ( splitPathWord
  , hasTrailingSlash
  , formatCompletion
  ) where

import Data.List (isSuffixOf)

-- | Split the word under the cursor into its directory prefix and base name.
{-@ splitPathWord :: word:String ->
      {v:(String, String) | len word == len (fst v) + len (snd v)} @-}
splitPathWord :: String -> (String, String)
splitPathWord word =
  case break (== '/') (reverse word) of
    (baseReversed, []) -> ("", reverse baseReversed)
    (baseReversed, _ : directoryReversed) ->
      (reverse directoryReversed ++ "/", reverse baseReversed)

hasTrailingSlash :: FilePath -> Bool
hasTrailingSlash = isSuffixOf "/"

-- | Add a trailing slash to completed directory entries.
{-@ formatCompletion :: candidate:FilePath -> isDirectory:Bool ->
      {v:FilePath | len v == len candidate + (if isDirectory then 1 else 0)} @-}
formatCompletion :: FilePath -> Bool -> FilePath
formatCompletion candidate isDirectory =
  candidate ++ if isDirectory then "/" else ""
