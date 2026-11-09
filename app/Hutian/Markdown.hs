-- | Lightweight streaming ANSI highlighter for Markdown replies.
module Hutian.Markdown
  ( MarkdownState
  , initialMarkdownState
  , renderMarkdown
  , renderMarkdownWith
  , finishMarkdown
  , finishMarkdownWith
  ) where

import Data.Char (isControl, isDigit, isSpace)
import Data.List (intercalate, isPrefixOf, mapAccumL)
import qualified Data.Text as Text
import Skylighting
  ( ANSIColorLevel (..)
  , FormatOptions (..)
  , Style (..)
  , TokenizerConfig (..)
  , breezeDark
  , defaultFormatOpts
  , defaultSyntaxMap
  , formatANSI
  , lookupSyntax
  , tokenize
  )

data MarkdownState = MarkdownState
  { markdownFence :: Maybe Fence
  , markdownFenceLanguage :: String
  , markdownFenceLines :: [String]
  , markdownPartial :: String
  }

data Fence = Fence
  { fenceCharacter :: Char
  , fenceLength :: Int
  }

initialMarkdownState :: MarkdownState
initialMarkdownState = MarkdownState Nothing "" [] ""

-- | Highlight a chunk of Markdown. Complete lines are emitted; an incomplete
-- trailing line is kept until the next chunk or 'finishMarkdown'.
renderMarkdown :: MarkdownState -> Text.Text -> (MarkdownState, String)
renderMarkdown = renderMarkdownWith ""

renderMarkdownWith :: String -> MarkdownState -> Text.Text -> (MarkdownState, String)
renderMarkdownWith base state chunk =
  let (completeLines, partial) =
        splitCompleteLines (markdownPartial state ++ Text.unpack chunk)
      (state', renderedLines) =
        mapAccumL
          (renderLine base)
          (state { markdownPartial = partial })
          completeLines
   in (state', unlines (concat renderedLines))

finishMarkdown :: MarkdownState -> String
finishMarkdown = finishMarkdownWith ""

finishMarkdownWith :: String -> MarkdownState -> String
finishMarkdownWith base state =
  let (afterPartial, partialLines) =
        case markdownPartial state of
          "" -> (state, [])
          line -> renderLine base state line
      (_, fenceLines) = flushFence base afterPartial
   in intercalate "\n" (partialLines ++ fenceLines)

splitCompleteLines :: String -> ([String], String)
splitCompleteLines input =
  case break (== '\n') input of
    (_, []) -> ([], input)
    (line, _ : rest) ->
      let (lines', remainder) = splitCompleteLines rest
       in (line : lines', remainder)

renderLine :: String -> MarkdownState -> String -> (MarkdownState, [String])
renderLine base state line =
  case markdownFence state of
    Just fence
      | isFenceClose fence line ->
          let (next, codeLines) = flushFence base state
           in (next, codeLines ++ [renderOutput base (styled fenceColour line)])
      | otherwise ->
          ( state { markdownFenceLines = line : markdownFenceLines state }
          , []
          )
    Nothing
      | Just (fence, language) <- fenceStart line ->
          ( state
              { markdownFence = Just fence
              , markdownFenceLanguage = language
              , markdownFenceLines = []
              }
          , [renderOutput base (styled fenceColour line)]
          )
      | otherwise -> (state, [renderOutput base (renderNormal base line)])

renderOutput :: String -> String -> String
renderOutput base rendered = base ++ rendered ++ reset

flushFence :: String -> MarkdownState -> (MarkdownState, [String])
flushFence base state =
  case markdownFence state of
    Nothing -> (state, [])
    Just _ ->
      let codeLines = reverse (markdownFenceLines state)
          renderedLines = highlightCode (markdownFenceLanguage state) codeLines
       in ( state
              { markdownFence = Nothing
              , markdownFenceLanguage = ""
              , markdownFenceLines = []
              }
          , map (renderOutput base) renderedLines
          )

renderNormal :: String -> String -> String
renderNormal _ "" = ""
renderNormal base line
  | isHorizontalRule line = styled dimColour line
  | Just level <- headingLevel line = headingColour level ++ sanitize line
  | isBlockQuote line = blockQuoteColour ++ sanitize line
  | Just rendered <- renderList base line = rendered
  | otherwise = highlightInline base line

headingLevel :: String -> Maybe Int
headingLevel line =
  case dropWhile (== ' ') line of
    '#' : _ ->
      let (hashes, rest) = span (== '#') (dropWhile (== ' ') line)
       in if spaceFollows rest
            then Just (min 6 (length hashes))
            else Nothing
    _ -> Nothing

isHorizontalRule :: String -> Bool
isHorizontalRule line =
  let compact = filter (not . isSpace) line
   in length compact >= 3 && all (`elem` "-*_") compact

isBlockQuote :: String -> Bool
isBlockQuote line =
  case dropWhile (== ' ') line of
    '>' : _ -> True
    _ -> False

spaceFollows :: String -> Bool
spaceFollows value =
  case value of
    [] -> True
    ' ' : _ -> True
    _ -> False

renderList :: String -> String -> Maybe String
renderList base line =
  let (indent, rest) = span (== ' ') line
      restore = restoreForeground base
   in case rest of
        marker : after
          | marker `elem` ("-*+" :: String)
          , spaceFollows after ->
              Just (indent ++ markerColour ++ [marker] ++ restore ++ highlightInline base after)
        _ ->
          let (digits, afterDigits) = span isDigit rest
           in case afterDigits of
                delimiter : after
                  | not (null digits)
                  , delimiter `elem` (".)" :: String)
                  , spaceFollows after ->
                      Just
                        ( indent
                            ++ markerColour
                            ++ digits
                            ++ [delimiter]
                            ++ restore
                            ++ highlightInline base after
                        )
                _ -> Nothing

fenceStart :: String -> Maybe (Fence, String)
fenceStart line =
  case dropWhile (== ' ') line of
    character : _
      | character == '`' || character == '~' ->
          let (run, afterRun) = span (== character) (dropWhile (== ' ') line)
              language =
                normalizeLanguage
                  (takeWhile (not . isSpace) (dropWhile isSpace afterRun))
           in if length run >= 3
                then Just (Fence character (length run), language)
                else Nothing
    _ -> Nothing

normalizeLanguage :: String -> String
normalizeLanguage value =
  let inner =
        case value of
          '{' : rest -> takeWhile (/= '}') rest
          _ -> value
      firstWord = takeWhile (not . isSpace) inner
   in dropWhile (== '.') firstWord

isFenceClose :: Fence -> String -> Bool
isFenceClose fence line =
  let (run, rest) = span (== fenceCharacter fence) (dropWhile (== ' ') line)
   in length run >= fenceLength fence && all isSpace rest

highlightCode :: String -> [String] -> [String]
highlightCode _ [] = []
highlightCode language codeLines =
  case lookupSyntax (Text.pack language) defaultSyntaxMap of
    Nothing -> fallback
    Just syntax ->
      case
          tokenize
            (TokenizerConfig defaultSyntaxMap False)
            syntax
            (Text.pack (intercalate "\n" (map sanitize codeLines)))
        of
          Left _ -> fallback
          Right sourceLines ->
            case lines (Text.unpack (formatANSI codeFormatOptions codeStyle sourceLines)) of
              [] -> fallback
              rendered -> rendered
  where
    fallback = map (styled codeColour) codeLines

codeFormatOptions :: FormatOptions
codeFormatOptions = defaultFormatOpts { ansiColorLevel = ANSITrueColor }

codeStyle :: Style
codeStyle =
  breezeDark
    { defaultColor = Nothing
    , backgroundColor = Nothing
    }

highlightInline :: String -> String -> String
highlightInline _ [] = ""
highlightInline base ('`' : rest) =
  case break (== '`') rest of
    (code, '`' : after) ->
      codeColour
        ++ "`"
        ++ sanitize code
        ++ "`"
        ++ restoreForeground base
        ++ highlightInline base after
    _ -> '`' : highlightInline base rest
highlightInline base ('*' : '*' : rest) =
  case breakOn "**" rest of
    Just (body, after) ->
      boldColour
        ++ "**"
        ++ sanitize body
        ++ "**"
        ++ boldOff base
        ++ highlightInline base after
    Nothing -> "**" ++ highlightInline base rest
highlightInline base ('_' : '_' : rest) =
  case breakOn "__" rest of
    Just (body, after) ->
      boldColour
        ++ "__"
        ++ sanitize body
        ++ "__"
        ++ boldOff base
        ++ highlightInline base after
    Nothing -> "__" ++ highlightInline base rest
highlightInline base ('~' : '~' : rest) =
  case breakOn "~~" rest of
    Just (body, after) ->
      strikeColour
        ++ "~~"
        ++ sanitize body
        ++ "~~"
        ++ strikeOff
        ++ highlightInline base after
    Nothing -> "~~" ++ highlightInline base rest
highlightInline base ('*' : rest) =
  case break (== '*') rest of
    (body, '*' : after) ->
      italicColour
        ++ "*"
        ++ sanitize body
        ++ "*"
        ++ italicOff
        ++ highlightInline base after
    _ -> '*' : highlightInline base rest
highlightInline base ('_' : rest) =
  case break (== '_') rest of
    (body, '_' : after) ->
      italicColour
        ++ "_"
        ++ sanitize body
        ++ "_"
        ++ italicOff
        ++ highlightInline base after
    _ -> '_' : highlightInline base rest
highlightInline base ('[' : rest) =
  case break (== ']') rest of
    (text, ']' : '(' : after) ->
      case break (== ')') after of
        (url, ')' : more) ->
          linkColour
            ++ "["
            ++ sanitize text
            ++ "]("
            ++ sanitize url
            ++ ")"
            ++ "\ESC[24m"
            ++ restoreForeground base
            ++ highlightInline base more
        _ -> '[' : highlightInline base rest
    _ -> '[' : highlightInline base rest
highlightInline base ('\\' : character : rest) =
  '\\' : sanitizeChar character : highlightInline base rest
highlightInline base (character : rest) =
  sanitizeChar character : highlightInline base rest

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go ""
  where
    go _ [] = Nothing
    go prefix input@(character : rest)
      | needle `isPrefixOf` input =
          Just (reverse prefix, drop (length needle) input)
      | otherwise = go (character : prefix) rest

sanitize :: String -> String
sanitize = map sanitizeChar

sanitizeChar :: Char -> Char
sanitizeChar character
  | isControl character && character /= '\t' = ' '
  | otherwise = character

styled :: String -> String -> String
styled colour value = colour ++ sanitize value

restoreForeground :: String -> String
restoreForeground base
  | null base = "\ESC[39m"
  | otherwise = base

reset, boldColour, italicColour, italicOff :: String
reset = "\ESC[0m"
boldColour = "\ESC[1;38;5;222m"
italicColour = "\ESC[3m"
italicOff = "\ESC[23m"

boldOff :: String -> String
boldOff base = "\ESC[22m" ++ restoreForeground base

strikeColour, strikeOff, codeColour, fenceColour, dimColour :: String
strikeColour = "\ESC[9m"
strikeOff = "\ESC[29m"
codeColour = "\ESC[38;5;220m"
fenceColour = "\ESC[38;5;81m"
dimColour = "\ESC[2;38;5;245m"

blockQuoteColour, markerColour, linkColour :: String
blockQuoteColour = "\ESC[38;5;244m"
markerColour = "\ESC[1;38;5;214m"
linkColour = "\ESC[4;38;5;81m"

headingColour :: Int -> String
headingColour level =
  "\ESC[1m" ++ case level of
    1 -> "\ESC[38;5;81m"
    2 -> "\ESC[38;5;117m"
    3 -> "\ESC[38;5;153m"
    4 -> "\ESC[38;5;183m"
    5 -> "\ESC[38;5;219m"
    _ -> "\ESC[38;5;245m"