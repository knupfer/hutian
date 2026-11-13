-- | Skylighting-based syntax highlighting for shell commands typed or shown in
-- the interactive prompt.
module Hutian.Syntax (highlightShellLine) where

import Data.Char (isControl)
import qualified Data.Text as Text
import Skylighting
  ( ANSIColorLevel (..)
  , Color (..)
  , FormatOptions (..)
  , Style (..)
  , TokenizerConfig (..)
  , defaultFormatOpts
  , defaultSyntaxMap
  , lookupSyntax
  , tokenize
  , zenburn
  )
import Skylighting.Format.ANSI (formatANSI)

-- | Highlight one shell command row. The visible text is preserved; only ANSI
-- styles are added. Control characters are neutralized so pasted escape
-- sequences cannot reach the terminal.
highlightShellLine :: String -> String
highlightShellLine "" = ""
highlightShellLine line =
  case lookupSyntax (Text.pack "bash") defaultSyntaxMap of
    Nothing -> plain
    Just syntax ->
      case tokenize (TokenizerConfig defaultSyntaxMap False) syntax (Text.pack plain) of
        Left _ -> plain
        Right sourceLines ->
          case lines (Text.unpack (formatANSI formatOptions shellStyle sourceLines)) of
            [rendered] -> rendered
            _ -> plain
  where
    plain = sanitize line

formatOptions :: FormatOptions
formatOptions = defaultFormatOpts {ansiColorLevel = ANSITrueColor}

shellStyle :: Style
shellStyle =
  zenburn
    { defaultColor = Just (RGB 255 255 255)
    , backgroundColor = Nothing
    }

sanitize :: String -> String
sanitize = map sanitizeChar

sanitizeChar :: Char -> Char
sanitizeChar character
  | isControl character && character /= '\t' = ' '
  | otherwise = character
