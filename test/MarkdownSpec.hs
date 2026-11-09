{-# LANGUAGE OverloadedStrings #-}

module MarkdownSpec (tests) where

import Data.Char (isControl)
import Data.List (isInfixOf, isSuffixOf)
import Hutian.Markdown
import qualified Data.Text as Text
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "markdown"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "headings are highlighted without changing the text" $ do
        let rendered = renderAll "# Heading\n"
        assertBool "expected ANSI heading style" ("\ESC[38;5;81m" `isInfixOf` rendered)
        stripAnsi rendered @?= "# Heading\n"
    , testCase "inline emphasis and code are highlighted" $ do
        let source = "Use **bold**, *italic*, and `code`.\n"
            rendered = renderAll source
        assertBool "expected bold style" ("\ESC[1;38;5;222m" `isInfixOf` rendered)
        assertBool "expected italic style" ("\ESC[3m" `isInfixOf` rendered)
        assertBool "expected code style" ("\ESC[38;5;220m" `isInfixOf` rendered)
        stripAnsi rendered @?= source
    , testCase "fenced code blocks stay highlighted across lines and chunks" $ do
        let (state1, output1) = renderMarkdown initialMarkdownState "```\ncode\n"
            (state2, output2) = renderMarkdown state1 "```\nafter\n"
            rendered = output1 ++ output2 ++ finishMarkdown state2
        stripAnsi rendered @?= "```\ncode\n```\nafter\n"
        assertBool "expected fence style" ("\ESC[38;5;81m" `isInfixOf` rendered)
    , testCase "incomplete lines are buffered until the next chunk" $ do
        let (state1, output1) = renderMarkdown initialMarkdownState "hel"
            (state2, output2) = renderMarkdown state1 "lo\n"
        output1 @?= ""
        stripAnsi (output2 ++ finishMarkdown state2) @?= "hello\n"
    , testCase "control characters from the model are neutralized" $ do
        stripAnsi (renderAll "a\ESC[31mb") @?= "a [31mb"
    , testCase "list markers are colored" $ do
        let rendered = renderAll "- item\n1. ordered\n"
        assertBool "expected marker style" ("\ESC[1;38;5;214m" `isInfixOf` rendered)
        stripAnsi rendered @?= "- item\n1. ordered\n"
    , testCase "all heading levels use distinct styles" $
        mapM_
          (\(level, source, colour) -> do
             let rendered = renderAll source
             assertBool
               ("expected heading level " ++ show level ++ " style")
               (colour `isInfixOf` rendered)
             stripAnsi rendered @?= source
          )
          [ (1 :: Int, "# one\n", "\ESC[38;5;81m")
          , (2, "## two\n", "\ESC[38;5;117m")
          , (3, "### three\n", "\ESC[38;5;153m")
          , (4, "#### four\n", "\ESC[38;5;183m")
          , (5, "##### five\n", "\ESC[38;5;219m")
          , (6, "###### six\n", "\ESC[38;5;245m")
          ]
    , testCase "blockquotes and horizontal rules are highlighted" $ do
        let quoted = renderAll "> quoted\n"
            rule = renderAll "---\n"
        assertBool "expected blockquote style" ("\ESC[38;5;244m" `isInfixOf` quoted)
        assertBool "expected rule style" ("\ESC[2;38;5;245m" `isInfixOf` rule)
        stripAnsi quoted @?= "> quoted\n"
        stripAnsi rule @?= "---\n"
    , testCase "underscore italics, strikethrough, and links are highlighted" $ do
        let source = "_italic_ ~~gone~~ [text](https://example.test)\n"
            rendered = renderAll source
        assertBool "expected italic style" ("\ESC[3m" `isInfixOf` rendered)
        assertBool "expected strikethrough style" ("\ESC[9m" `isInfixOf` rendered)
        assertBool "expected link style" ("\ESC[4;38;5;81m" `isInfixOf` rendered)
        stripAnsi rendered @?= source
    , testCase "escaped emphasis markers are not highlighted" $ do
        let rendered = renderAll "\\*literal\\*\n"
        assertBool "escaped asterisks should stay plain" (not ("\ESC[3m" `isInfixOf` rendered))
        stripAnsi rendered @?= "\\*literal\\*\n"
    , testCase "alternative list and fence markers are supported" $ do
        let lists = renderAll "* star\n+ plus\n2) paren\n"
            fence = renderAll "~~~\nbody\n~~~\n"
        assertBool "expected marker style" ("\ESC[1;38;5;214m" `isInfixOf` lists)
        assertBool "expected fence marker style" ("\ESC[38;5;81m" `isInfixOf` fence)
        assertBool "expected code body style" ("\ESC[38;5;220m" `isInfixOf` fence)
        stripAnsi lists @?= "* star\n+ plus\n2) paren\n"
        stripAnsi fence @?= "~~~\nbody\n~~~\n"
    , testCase "empty lines and Unicode are preserved" $ do
        let source = "# Héllo 👋\n\nplain\n"
            rendered = renderAll source
        stripAnsi rendered @?= source
    , testCase "a custom base color applies to thinking text" $ do
        let source = "plain **bold** and `code`\n"
            rendered = renderAllWith "\ESC[38;5;245m" source
        assertBool "expected grey base color" ("\ESC[38;5;245m" `isInfixOf` rendered)
        stripAnsi rendered @?= source
    , testCase "fenced code uses its language lexer" $ do
        let source = "```haskell\nmain = putStrLn \"hi\"\n```\n"
            plain = renderAll "```\nmain = putStrLn \"hi\"\n```\n"
            haskell = renderAll source
        assertBool "language-specific highlighting should differ from plain fallback" (haskell /= plain)
        stripAnsi haskell @?= source
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "highlighting preserves printable source text" prop_preservesText
    , testProperty "chunk boundaries do not change highlighting" prop_chunking
    , testProperty "single-character chunks produce the same highlighting" prop_characterChunks
    , testProperty "styles are reset by the end of each emitted line" prop_resetsStyles
    , testProperty "control characters are neutralized" prop_sanitizesControls
    , testProperty "a base color does not change the source text" prop_basePreservesText
    ]

renderAll :: String -> String
renderAll = renderAllWith ""

renderAllWith :: String -> String -> String
renderAllWith base source =
  let (state, rendered) = renderMarkdownWith base initialMarkdownState (Text.pack source)
   in rendered ++ finishMarkdownWith base state

stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC' : '[' : rest) = stripAnsi (dropSequence rest)
  where
    dropSequence [] = []
    dropSequence (character : characters)
      | character >= '@' && character <= '~' = characters
      | otherwise = dropSequence characters
stripAnsi (character : rest) = character : stripAnsi rest

prop_preservesText :: String -> Property
prop_preservesText source =
  let clean = filter (not . isControl) source
   in stripAnsi (renderAll clean) === clean

prop_chunking :: String -> Property
prop_chunking source =
  let clean = filter (not . isControl) source
      (first, second) = splitAt (length clean `div` 2) clean
      (state1, output1) = renderMarkdown initialMarkdownState (Text.pack first)
      (state2, output2) = renderMarkdown state1 (Text.pack second)
      chunked = output1 ++ output2 ++ finishMarkdown state2
   in chunked === renderAll clean

prop_characterChunks :: String -> Property
prop_characterChunks source =
  let clean = filter (not . isControl) source
      (state, rendered) = foldl renderCharacter (initialMarkdownState, "") clean
   in rendered ++ finishMarkdown state === renderAll clean
  where
    renderCharacter (state, rendered) character =
      let (next, chunkOutput) = renderMarkdown state (Text.pack [character])
       in (next, rendered ++ chunkOutput)

prop_resetsStyles :: String -> Property
prop_resetsStyles source =
  let rendered = renderAll (filter (not . isControl) source)
      outputLines = lines rendered
   in counterexample (show outputLines) $
        conjoin (map (property . ("\ESC[0m" `isSuffixOf`)) outputLines)

prop_basePreservesText :: String -> Property
prop_basePreservesText source =
  let clean = filter (not . isControl) source
   in stripAnsi (renderAllWith "\ESC[38;5;245m" clean) === clean

prop_sanitizesControls :: String -> Property
prop_sanitizesControls source =
  counterexample (show rendered) $
    conjoin (map (property . allowed) rendered)
  where
    rendered = stripAnsi (renderAll source)
    allowed character =
      not (isControl character)
        || character == '\n'
        || character == '\t'