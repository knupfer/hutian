-- | Pure path display and expansion helpers.
module Hutian.Paths
  ( displayPath
  , expandHome
  ) where

import Data.List (stripPrefix)
import System.FilePath ((</>))

displayPath :: FilePath -> FilePath -> FilePath
displayPath home cwd =
  case stripPrefix home cwd of
    Just "" -> "~"
    Just rest -> "~" ++ rest
    Nothing -> cwd

expandHome :: FilePath -> FilePath -> FilePath
expandHome home target
  | target == "~" = home
  | Just rest <- stripPrefix "~/" target = home </> rest
  | otherwise = target