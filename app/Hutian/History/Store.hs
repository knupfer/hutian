{-# LANGUAGE ScopedTypeVariables #-}

-- | Persistent storage for interactive history.
module Hutian.History.Store
  ( HistoryFile (..)
  , loadHistoryFile
  , appendHistoryFile
  ) where

import Control.Exception (IOException, catch)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Hutian.History (decodeHistory, encodeHistory)

-- | A file that stores history entries in the line-oriented on-disk format.
newtype HistoryFile = HistoryFile
  { historyFilePath :: FilePath
  }
  deriving stock (Eq, Show)

-- | Read a history file. A missing or unreadable file is treated as empty
-- history so that an unwritable home directory never prevents startup.
loadHistoryFile :: HistoryFile -> IO [String]
loadHistoryFile (HistoryFile path) = do
  contents <- TextIO.readFile path `catch` \(_ :: IOException) -> pure Text.empty
  pure (decodeHistory (Text.unpack contents))

-- | Append one entry to a history file. Storage failures are intentionally
-- ignored: losing history must not interrupt an interactive session.
appendHistoryFile :: HistoryFile -> String -> IO ()
appendHistoryFile (HistoryFile path) text =
  TextIO.appendFile path (Text.pack (encodeHistory [text]))
    `catch` \(_ :: IOException) -> pure ()
