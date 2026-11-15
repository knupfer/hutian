-- | Shared Skylighting integration for ANSI syntax highlighting.
module Hutian.Highlight
  ( codeStyle
  , highlightLines
  , highlightShellLine
  , sanitize
  , sanitizeChar
  ) where

import Data.Char (isControl)
import Data.List (intercalate)
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

-- | The base syntax style used for fenced code and shell input.
codeStyle :: Style
codeStyle =
  zenburn
    { defaultColor = Just (RGB 255 255 255)
    , backgroundColor = Nothing
    }

-- | Highlight source lines in the named language. 'Nothing' means the
-- language is unknown or tokenization failed, so callers can fall back to
-- their own styling.
highlightLines :: Style -> String -> [String] -> Maybe [String]
highlightLines style language sourceLines = do
  syntax <- lookupSyntax (Text.pack language) defaultSyntaxMap
  tokenized <- either (const Nothing) Just (tokenize tokenizer syntax source)
  pure (lines (Text.unpack (formatANSI formatOptions style tokenized)))
  where
    tokenizer = TokenizerConfig defaultSyntaxMap False
    source = Text.pack (intercalate "\n" (map sanitize sourceLines))

formatOptions :: FormatOptions
formatOptions = defaultFormatOpts {ansiColorLevel = ANSITrueColor}

-- | Highlight one shell command row. The visible text is preserved; only ANSI
-- styles are added. Control characters are neutralized so pasted escape
-- sequences cannot reach the terminal.
highlightShellLine :: String -> String
highlightShellLine "" = ""
highlightShellLine line =
  case highlightLines codeStyle "bash" [line] of
    Just [rendered] -> rendered
    _ -> sanitize line

-- | Replace control characters except tabs with spaces so terminal escape
-- sequences cannot leak through.
sanitize :: String -> String
sanitize = map sanitizeChar

sanitizeChar :: Char -> Char
sanitizeChar character
  | isControl character && character /= '\t' = ' '
  | otherwise = character
