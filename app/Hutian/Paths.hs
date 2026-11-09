-- | Pure path display, expansion, and completion helpers.
module Hutian.Paths
  ( displayPath
  , expandHome
  , splitPathWord
  , hasTrailingSlash
  ) where

import Data.List (stripPrefix)
import System.FilePath ((</>))

{-@ displayPath :: home:{v:FilePath | len v > 0} -> cwd:{v:FilePath | len v > 0} -> {v:FilePath | len v > 0} @-}
displayPath :: FilePath -> FilePath -> FilePath
displayPath home cwd
  | cwd == home = "~"
  | Just rest@('/' : _) <- stripPrefix home cwd = "~" ++ rest
  | otherwise = cwd

expandHome :: FilePath -> FilePath -> FilePath
expandHome home target
  | target == "~" = home
  | Just rest <- stripPrefix "~/" target = home </> rest
  | otherwise = target

-- | Split the word under the cursor into its directory prefix and base name.
splitPathWord :: String -> (String, String)
splitPathWord word =
  case break (== '/') (reverse word) of
    (baseReversed, []) -> ("", reverse baseReversed)
    (baseReversed, _ : directoryReversed) ->
      (reverse directoryReversed ++ "/", reverse baseReversed)

hasTrailingSlash :: FilePath -> Bool
hasTrailingSlash value =
  case reverse value of
    '/' : _ -> True
    _ -> False