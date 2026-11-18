-- | Typed terminal colours and styles.
--
-- The rest of the program describes colours and attributes as values. Only
-- 'styleCode' turns them into an ANSI escape sequence.
module Hutian.Ansi
  ( Colour
  , rgb
  , colourChannels
  , darkenColour
  , Style
  , styleCode
  , darkenStyle
  , bold
  , faint
  , italic
  , underline
  , foreground
  , background
  , reset
  , defaultForeground
  , normalIntensity
  , noItalic
  , noUnderline
  ) where

import Data.Colour.SRGB (RGB (..), sRGB24, toSRGB24)
import qualified Data.Colour.SRGB as SRGB
import Data.Word (Word8)
import System.Console.ANSI
  ( ConsoleIntensity (..)
  , ConsoleLayer (..)
  , SGR (..)
  , Underlining (..)
  , setSGRCode
  )

-- | A 24-bit RGB colour.
type Colour = RGB Word8

rgb :: Word8 -> Word8 -> Word8 -> Colour
rgb = RGB

colourChannels :: Colour -> (Word8, Word8, Word8)
colourChannels (RGB red green blue) = (red, green, blue)

-- | Darken every channel by a factor. Factors at or above one leave the
-- colour unchanged, matching the old string-based implementation.
darkenColour :: Double -> Colour -> Colour
darkenColour factor colour@(RGB red green blue)
  | factor >= 1 = colour
  | otherwise =
      RGB
        (scaleChannel factor red)
        (scaleChannel factor green)
        (scaleChannel factor blue)

scaleChannel :: Double -> Word8 -> Word8
scaleChannel factor channel =
  fromIntegral (max 0 (min 255 (round (fromIntegral channel * factor) :: Int)))

-- | A sequence of ANSI Select Graphic Rendition attributes.
newtype Style = Style [SGR]

instance Semigroup Style where
  Style left <> Style right = Style (left <> right)

instance Monoid Style where
  mempty = Style []

-- | Encode a style. An empty style emits nothing rather than an empty CSI
-- sequence.
styleCode :: Style -> String
styleCode (Style []) = ""
styleCode (Style commands) = setSGRCode commands

-- | Darken the RGB components of a style, leaving attributes and resets alone.
darkenStyle :: Double -> Style -> Style
darkenStyle factor (Style commands) = Style (map darkenCommand commands)
  where
    darkenCommand command =
      case command of
        SetRGBColor layer colour ->
          SetRGBColor layer (toColour (darkenColour factor (fromColour colour)))
        _ -> command

bold, faint, italic, underline :: Style
bold = Style [SetConsoleIntensity BoldIntensity]
faint = Style [SetConsoleIntensity FaintIntensity]
italic = Style [SetItalicized True]
underline = Style [SetUnderlining SingleUnderline]

foreground, background :: Colour -> Style
foreground = Style . pure . SetRGBColor Foreground . toColour
background = Style . pure . SetRGBColor Background . toColour

toColour :: Colour -> SRGB.Colour Float
toColour (RGB red green blue) = sRGB24 red green blue

fromColour :: SRGB.Colour Float -> Colour
fromColour colour =
  case toSRGB24 colour of
    RGB red green blue -> RGB red green blue

reset :: Style
reset = Style [Reset]

defaultForeground :: Style
defaultForeground = Style [SetDefaultColor Foreground]

normalIntensity :: Style
normalIntensity = Style [SetConsoleIntensity NormalIntensity]

noItalic :: Style
noItalic = Style [SetItalicized False]

noUnderline :: Style
noUnderline = Style [SetUnderlining NoUnderline]
