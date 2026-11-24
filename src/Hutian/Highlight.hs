-- | Shared Skylighting integration for ANSI syntax highlighting.
module Hutian.Highlight
  ( codeStyle
  , dimCodeStyle
  , highlightLines
  , highlightShellLine
  , highlightJsonText
  , sanitize
  , sanitizeChar
  , sanitizeOutput
  , sanitizeOutputBytes
  ) where

import qualified Data.ByteString as BS
import Data.Char (isControl)
import Data.List (intercalate)
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Hutian.Ansi as Ansi
import Skylighting
  ( ANSIColorLevel (..)
  , Color (..)
  , FormatOptions (..)
  , Style (..)
  , TokenizerConfig (..)
  , TokenStyle (..)
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

-- | Darken every colour in a Skylighting style. The Markdown renderer uses the
-- same factor for dim thinking output.
dimCodeStyle :: Double -> Style -> Style
dimCodeStyle factor style =
  style
    { tokenStyles = fmap dimTokenStyle (tokenStyles style)
    , defaultColor = fmap dim (defaultColor style)
    , backgroundColor = fmap dim (backgroundColor style)
    , lineNumberColor = fmap dim (lineNumberColor style)
    , lineNumberBackgroundColor = fmap dim (lineNumberBackgroundColor style)
    }
  where
    dim = dimSkylightingColour factor
    dimTokenStyle tokenStyle =
      tokenStyle
        { tokenColor = fmap dim (tokenColor tokenStyle)
        , tokenBackground = fmap dim (tokenBackground tokenStyle)
        }

dimSkylightingColour :: Double -> Color -> Color
dimSkylightingColour factor (RGB red green blue) =
  let (red', green', blue') =
        Ansi.colourChannels
          (Ansi.darkenColour factor (Ansi.rgb red green blue))
   in RGB red' green' blue'

-- | Highlight a JSON tool argument with the same dimming used for thinking
-- output. The text is returned unchanged apart from ANSI styles.
highlightJsonText :: String -> String
highlightJsonText text =
  case highlightLines (dimCodeStyle Ansi.dimFactor codeStyle) "json" (lines text) of
    Just rendered | not (null rendered) -> intercalate "\n" rendered
    _ -> sanitizeOutput text

-- | Replace control characters except tabs with spaces so terminal escape
-- sequences cannot leak through.
sanitize :: String -> String
sanitize = map sanitizeChar

sanitizeChar :: Char -> Char
sanitizeChar character
  | isControl character && character /= '\t' = ' '
  | otherwise = character

-- | Neutralize control characters in multi-line command, job, or tool output.
-- Unlike 'sanitize', newlines are preserved so captured output keeps its
-- layout; carriage returns and escape sequences still become spaces.
sanitizeOutput :: String -> String
sanitizeOutput = map sanitizeOutputChar

sanitizeOutputChar :: Char -> Char
sanitizeOutputChar character
  | isControl character && character /= '\t' && character /= '\n' = ' '
  | otherwise = character

-- | Byte-level 'sanitizeOutput' for streamed command output. Working on bytes
-- keeps multi-byte UTF-8 sequences intact even when a chunk boundary splits
-- them; only control bytes change.
sanitizeOutputBytes :: BS.ByteString -> BS.ByteString
sanitizeOutputBytes = BS.map sanitizeOutputByte

sanitizeOutputByte :: Word8 -> Word8
sanitizeOutputByte byte
  | byte == 0x09 || byte == 0x0A = byte
  | byte < 0x20 || byte == 0x7F = 0x20
  | otherwise = byte
