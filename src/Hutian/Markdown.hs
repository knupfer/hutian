-- | Lightweight streaming ANSI highlighter for Markdown replies.
module Hutian.Markdown
  ( MarkdownFlavor (..)
  , MarkdownState
  , initialMarkdownState
  , renderMarkdown
  , renderMarkdownWith
  , renderMarkdownRows
  , finishMarkdown
  , finishMarkdownWith
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isPrefixOf, mapAccumL)
import qualified Data.Text as Text
import qualified Hutian.Ansi as Ansi
import Hutian.Highlight (codeStyle, dimCodeStyle, highlightLines, sanitize, sanitizeChar)

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

-- | Whether output uses the normal palette or a dim base colour, as used by
-- reasoning blocks and subagent answers.
data MarkdownFlavor
  = NormalMarkdown
  | DimMarkdown String
  deriving stock (Eq, Show)

-- | Highlight a chunk of Markdown. Complete lines are emitted; an incomplete
-- trailing line is kept until the next chunk or 'finishMarkdown'.
renderMarkdown :: MarkdownState -> Text.Text -> (MarkdownState, String)
renderMarkdown = renderMarkdownWith NormalMarkdown

renderMarkdownWith :: MarkdownFlavor -> MarkdownState -> Text.Text -> (MarkdownState, String)
renderMarkdownWith flavor state chunk =
  let context = contextFor flavor
      (completeLines, partial) =
        splitCompleteLines (markdownPartial state ++ Text.unpack chunk)
      (state', renderedLines) =
        mapAccumL
          (renderLine context)
          (state { markdownPartial = partial })
          completeLines
   in (state', unlines (concat renderedLines))

-- | Highlight already-wrapped Markdown rows for a live editor. Every input row
-- produces exactly one output row, so the result can be rendered while the
-- user is still typing. Fenced code blocks are highlighted across rows.
renderMarkdownRows :: [String] -> [String]
renderMarkdownRows = go
  where
    context = contextFor NormalMarkdown
    colours = contextColours context
    go [] = []
    go (row : rest) =
      case fenceStart row of
        Just (fence, language) ->
          let (body, afterFence) = break (isFenceClose fence) rest
              (closing, remaining) =
                case afterFence of
                  close : more -> ([close], more)
                  [] -> ([], [])
           in styled (coloursFence colours) row
                : highlightBody language body
                ++ map (styled (coloursFence colours)) closing
                ++ go remaining
        Nothing -> renderNormal context row : go rest

    highlightBody language body =
      let highlighted = highlightCode colours language body
       in if length highlighted == length body
            then highlighted
            else map (styled (coloursCode colours)) body

finishMarkdown :: MarkdownState -> String
finishMarkdown = finishMarkdownWith NormalMarkdown

finishMarkdownWith :: MarkdownFlavor -> MarkdownState -> String
finishMarkdownWith flavor state =
  let context = contextFor flavor
      (afterPartial, partialLines) =
        case markdownPartial state of
          "" -> (state, [])
          line -> renderLine context state line
      (_, fenceLines) = flushFence context afterPartial
   in intercalate "\n" (partialLines ++ fenceLines)

splitCompleteLines :: String -> ([String], String)
splitCompleteLines input =
  case break (== '\n') input of
    (_, []) -> ([], input)
    (line, _ : rest) ->
      let (lines', remainder) = splitCompleteLines rest
       in (line : lines', remainder)

renderLine :: MarkdownContext -> MarkdownState -> String -> (MarkdownState, [String])
renderLine context state line =
  case markdownFence state of
    Just fence
      | isFenceClose fence line ->
          let (next, codeLines) = flushFence context state
           in ( next
              , codeLines
                  ++ [renderOutput (contextBase context) (styled (coloursFence colours) line)]
              )
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
          , [renderOutput (contextBase context) (styled (coloursFence colours) line)]
          )
      | otherwise ->
          (state, [renderOutput (contextBase context) (renderNormal context line)])
  where
    colours = contextColours context

renderOutput :: String -> String -> String
renderOutput base rendered = base ++ rendered ++ reset

flushFence :: MarkdownContext -> MarkdownState -> (MarkdownState, [String])
flushFence context state =
  case markdownFence state of
    Nothing -> (state, [])
    Just _ ->
      let codeLines = reverse (markdownFenceLines state)
          renderedLines = highlightCode (contextColours context) (markdownFenceLanguage state) codeLines
       in ( state
              { markdownFence = Nothing
              , markdownFenceLanguage = ""
              , markdownFenceLines = []
              }
          , map (renderOutput (contextBase context)) renderedLines
          )

renderNormal :: MarkdownContext -> String -> String
renderNormal _ "" = ""
renderNormal context line
  | isHorizontalRule line = styled (coloursRule colours) line
  | Just level <- headingLevel line =
      Ansi.styleCode (coloursHeading colours level) ++ sanitize line
  | isBlockQuote line =
      Ansi.styleCode (coloursQuote colours) ++ sanitize line
  | Just rendered <- renderList context line = rendered
  | otherwise = highlightInline context line
  where
    colours = contextColours context

headingLevel :: String -> Maybe Int
headingLevel line =
  case dropWhile (== ' ') line of
    hashes@('#' : _) ->
      let (headingHashes, rest) = span (== '#') hashes
       in if spaceFollows rest
            then Just (min 6 (length headingHashes))
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

renderList :: MarkdownContext -> String -> Maybe String
renderList context line =
  let (indent, rest) = span (== ' ') line
      restore = restoreForeground (contextBase context)
      colours = contextColours context
      markerColour = Ansi.styleCode (coloursMarker colours)
   in case rest of
        marker : after
          | marker `elem` ("-*+" :: String)
          , spaceFollows after ->
              Just
                ( indent
                    ++ markerColour
                    ++ [marker]
                    ++ restore
                    ++ highlightInline context after
                )
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
                            ++ highlightInline context after
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

highlightCode :: MarkdownColours -> String -> [String] -> [String]
highlightCode _ _ [] = []
highlightCode colours language codeLines =
  case highlightLines styledCodeStyle language codeLines of
    Just rendered | not (null rendered) -> rendered
    _ -> fallback
  where
    styledCodeStyle = dimCodeStyle (coloursScale colours) codeStyle
    fallback = map (styled (coloursCode colours)) codeLines

highlightInline :: MarkdownContext -> String -> String
highlightInline context = go
  where
    base = contextBase context
    colours = contextColours context
    restore = restoreForeground base
    codeColour = Ansi.styleCode (coloursCode colours)
    boldColour = Ansi.styleCode (coloursBold colours)
    italicColour = Ansi.styleCode (coloursItalic colours)
    linkColour = Ansi.styleCode (coloursLink colours)

    go [] = ""
    go ('`' : rest) =
      case break (== '`') rest of
        (code, '`' : after) ->
          codeColour
            ++ "`"
            ++ sanitize code
            ++ "`"
            ++ restore
            ++ go after
        _ -> '`' : go rest
    go ('*' : '*' : rest) =
      emphasis "**" boldColour (boldOff base) rest
    go ('_' : '_' : rest) =
      emphasis "__" boldColour (boldOff base) rest
    go ('~' : '~' : rest) =
      emphasis "~~" Ansi.strikeCode Ansi.noStrikeCode rest
    go ('*' : rest) =
      emphasis "*" italicColour (italicOff base) rest
    go ('_' : rest) =
      emphasis "_" italicColour (italicOff base) rest
    go ('[' : rest) =
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
                ++ linkOff base
                ++ go more
            _ -> '[' : go rest
        _ -> '[' : go rest
    go ('\\' : character : rest) =
      '\\' : sanitizeChar character : go rest
    go (character : rest) =
      sanitizeChar character : go rest

    emphasis delimiter start end rest =
      case breakOn delimiter rest of
        Just (body, after) ->
          start ++ delimiter ++ sanitize body ++ delimiter ++ end ++ go after
        Nothing -> delimiter ++ go rest

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go ""
  where
    go _ [] = Nothing
    go prefix input@(character : rest)
      | needle `isPrefixOf` input =
          Just (reverse prefix, drop (length needle) input)
      | otherwise = go (character : prefix) rest

styled :: Ansi.Style -> String -> String
styled colour value = Ansi.styleCode colour ++ sanitize value

restoreForeground :: String -> String
restoreForeground base
  | null base = Ansi.styleCode Ansi.defaultForeground
  | otherwise = base

-- | Rendering context: the base colour for subordinate text and the palette to
-- use. Dim output gets a base colour and a dimmed accent palette.
data MarkdownContext = MarkdownContext
  { contextBase :: String
  , contextColours :: MarkdownColours
  }

contextFor :: MarkdownFlavor -> MarkdownContext
contextFor NormalMarkdown = MarkdownContext "" normalColours
contextFor (DimMarkdown base) = MarkdownContext base dimColours

data MarkdownColours = MarkdownColours
  { coloursBold :: Ansi.Style
  , coloursItalic :: Ansi.Style
  , coloursCode :: Ansi.Style
  , coloursFence :: Ansi.Style
  , coloursRule :: Ansi.Style
  , coloursQuote :: Ansi.Style
  , coloursMarker :: Ansi.Style
  , coloursLink :: Ansi.Style
  , coloursHeading :: Int -> Ansi.Style
  , coloursScale :: Double
  }

reset :: String
reset = Ansi.styleCode Ansi.reset

-- | The Markdown palette is grouped by role so related elements share a hue:
-- headings climb a blue-to-rose ramp, emphasis is warm yellow, code and fence
-- markers are green, links are underlined teal, list markers are peach, and
-- quotes and rules are muted grey.
normalColours :: MarkdownColours
normalColours =
  MarkdownColours
    { coloursBold = Ansi.bold <> Ansi.foreground (Ansi.rgb 249 226 175)
    , coloursItalic = Ansi.italic <> Ansi.foreground (Ansi.rgb 249 226 175)
    , coloursCode = Ansi.foreground (Ansi.rgb 166 227 161)
    , coloursFence = Ansi.foreground (Ansi.rgb 166 227 161)
    , coloursRule = Ansi.faint <> Ansi.foreground (Ansi.rgb 127 132 156)
    , coloursQuote = Ansi.foreground (Ansi.rgb 147 153 178)
    , coloursMarker = Ansi.bold <> Ansi.foreground (Ansi.rgb 250 179 135)
    , coloursLink = Ansi.underline <> Ansi.foreground (Ansi.rgb 148 226 213)
    , coloursHeading = headingColour
    , coloursScale = 1
    }

-- | Thinking output keeps the normal hues, scaled down so highlighting does
-- not compete with the base colour.
dimColours :: MarkdownColours
dimColours = darkenColours Ansi.dimFactor normalColours

darkenColours :: Double -> MarkdownColours -> MarkdownColours
darkenColours factor colours =
  MarkdownColours
    { coloursBold = Ansi.darkenStyle factor (coloursBold colours)
    , coloursItalic = Ansi.darkenStyle factor (coloursItalic colours)
    , coloursCode = Ansi.darkenStyle factor (coloursCode colours)
    , coloursFence = Ansi.darkenStyle factor (coloursFence colours)
    , coloursRule = Ansi.darkenStyle factor (coloursRule colours)
    , coloursQuote = Ansi.darkenStyle factor (coloursQuote colours)
    , coloursMarker = Ansi.darkenStyle factor (coloursMarker colours)
    , coloursLink = Ansi.darkenStyle factor (coloursLink colours)
    , coloursHeading = Ansi.darkenStyle factor . coloursHeading colours
    , coloursScale = factor
    }

boldOff :: String -> String
boldOff base = Ansi.styleCode Ansi.normalIntensity ++ restoreForeground base

italicOff :: String -> String
italicOff base = Ansi.styleCode Ansi.noItalic ++ restoreForeground base

linkOff :: String -> String
linkOff base = Ansi.styleCode Ansi.noUnderline ++ restoreForeground base

headingColour :: Int -> Ansi.Style
headingColour level =
  Ansi.bold
    <> Ansi.foreground
      ( case level of
          1 -> Ansi.rgb 137 180 250
          2 -> Ansi.rgb 180 190 254
          3 -> Ansi.rgb 203 166 247
          4 -> Ansi.rgb 245 194 231
          5 -> Ansi.rgb 235 160 172
          _ -> Ansi.rgb 166 173 200
      )
