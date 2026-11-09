module EditorSpec (tests) where

import Data.Char (isControl)
import qualified Data.List.NonEmpty as NonEmpty
import Hutian.Editor
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "editor"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "unit"
    [ testCase "initial prompt is a single empty line" $ do
        lineList initialPrompt @?= [""]
        promptRow initialPrompt @?= Row 0
        promptColumn initialPrompt @?= Column 0
        promptGoal initialPrompt @?= Nothing
    , testCase "typing appends at the cursor" $ do
        let prompt = runText "abc"
        promptText prompt @?= "abc\n"
        cursor prompt @?= (0, 3)
    , testCase "Enter splits the current line at the cursor" $ do
        let prompt = run (text "abc" ++ [KeyLeft, KeyEnter])
        lineList prompt @?= ["ab", "c"]
        cursor prompt @?= (1, 0)
    , testCase "backspace merges a line with the previous one" $ do
        let prompt = run (text "a" ++ [KeyEnter] ++ text "b" ++ [KeyHome, KeyBackspace])
        lineList prompt @?= ["ab"]
        cursor prompt @?= (0, 1)
    , testCase "delete merges a line with the next one" $ do
        let prompt = run (text "a" ++ [KeyEnter] ++ text "b" ++ [KeyUp, KeyEnd, KeyDelete])
        lineList prompt @?= ["ab"]
        cursor prompt @?= (0, 1)
    , testCase "delete removes the character under the cursor" $ do
        let prompt = run (text "abc" ++ [KeyLeft, KeyLeft, KeyDelete])
        lineList prompt @?= ["ac"]
        cursor prompt @?= (0, 1)
    , testCase "left and right cross line boundaries" $ do
        let prompt = run (text "a" ++ [KeyEnter] ++ text "b" ++ [KeyHome])
            left = applyKey KeyLeft prompt
            right = applyKey KeyRight left
        cursor left @?= (0, 1)
        cursor right @?= (1, 0)
    , testCase "up clamps to the shorter line and keeps the goal column" $ do
        let prompt = run (text "abcdef" ++ [KeyEnter] ++ text "x" ++ [KeyEnter] ++ text "abcdef")
            upOnce = applyKey KeyUp prompt
            upTwice = applyKey KeyUp upOnce
        cursor prompt @?= (2, 6)
        cursor upOnce @?= (1, 1)
        promptGoal upOnce @?= Just (GoalColumn 6)
        cursor upTwice @?= (0, 6)
    , testCase "down keeps the goal column and stops at the last line" $ do
        let prompt = run (text "ab" ++ [KeyEnter] ++ text "x")
            up = applyKey KeyUp prompt
            down = applyKey KeyDown up
            downAgain = applyKey KeyDown down
        cursor up @?= (0, 1)
        promptGoal up @?= Just (GoalColumn 1)
        cursor down @?= (1, 1)
        promptGoal down @?= Just (GoalColumn 1)
        cursor downAgain @?= (1, 1)
    , testCase "horizontal movement clears the goal column" $ do
        let prompt = run (text "ab" ++ [KeyEnter] ++ text "x")
            up = applyKey KeyUp prompt
            down = applyKey KeyDown up
            left = applyKey KeyLeft down
        promptGoal down @?= Just (GoalColumn 1)
        promptGoal left @?= Nothing
    , testCase "Home and End move within the current line" $ do
        let prompt = run (text "abc" ++ [KeyHome, KeyEnd])
        cursor prompt @?= (0, 3)
    , testCase "mode toggles both ways" $ do
        toggleMode LlmMode @?= TermMode
        toggleMode TermMode @?= LlmMode
    , testCase "each mode keeps an independent prompt buffer" $ do
        let llmDraft = setPromptText "llm draft" initialPrompt
            termDraft = setPromptText "term draft" initialPrompt
            buffers =
              storePrompt
                TermMode
                termDraft
                (storePrompt LlmMode llmDraft initialPromptBuffers)
        promptText (activePrompt LlmMode buffers) @?= "llm draft\n"
        promptText (activePrompt TermMode buffers) @?= "term draft\n"
    , testCase "clearPrompt clears only the active mode" $ do
        let llmDraft = setPromptText "llm draft" initialPrompt
            termDraft = setPromptText "term draft" initialPrompt
            buffers =
              clearPrompt
                TermMode
                (storePrompt TermMode termDraft (storePrompt LlmMode llmDraft initialPromptBuffers))
        promptText (activePrompt LlmMode buffers) @?= "llm draft\n"
        promptText (activePrompt TermMode buffers) @?= "\n"
    , testCase "submitIntent follows the mode-specific keys" $ do
        let quoted = runText "echo 'open"
            continued = runText "echo \\"
        submitIntent LlmMode KeySubmit initialPrompt @?= SubmitNow
        submitIntent TermMode KeySubmit initialPrompt @?= NotSubmit
        submitIntent LlmMode KeyEnter initialPrompt @?= NotSubmit
        submitIntent TermMode KeyEnter initialPrompt @?= SubmitNow
        submitIntent TermMode KeyEnter quoted @?= ContinueLine
        submitIntent TermMode KeyEnter continued @?= ContinueLine
    , testCase "prompt text contains all lines" $ do
        let prompt = run (text "a" ++ [KeyEnter] ++ text "b")
        promptText prompt @?= "a\nb\n"
    , testCase "cursorToEnd moves to the last line end" $ do
        let prompt = run (text "a" ++ [KeyEnter] ++ text "b" ++ [KeyHome])
            ended = cursorToEnd prompt
        cursor ended @?= (1, 1)
        promptGoal ended @?= Nothing
    , testCase "wrapLine splits long and exact-width lines" $ do
        wrapLine 5 "abcdefgh" @?= ["abcde", "fgh"]
        wrapLine 3 "abcdef" @?= ["abc", "def"]
        wrapLine 3 "" @?= [""]
    , testCase "cursorPosition accounts for wrapping" $ do
        cursorPosition 3 "" (runText "abcdefg") @?= (2, 1)
    , testCase "physicalRows includes a row for a cursor at a wrap boundary" $ do
        physicalRows 3 "" (runText "aaaaaa") @?= ["aaa", "aaa", ""]
    , testCase "a prompt prefix is included in the first line layout" $ do
        let prompt = runText "abc"
        physicalRows 6 "~ $ " prompt @?= ["~ $ ab", "c"]
        cursorPosition 6 "~ $ " prompt @?= (1, 1)
    , testCase "the prompt prefix only appears on the first line" $ do
        let prompt = run (text "a" ++ [KeyEnter] ++ text "b")
        physicalRows 10 "> " prompt @?= ["> a", "b"]
        cursorPosition 10 "> " prompt @?= (1, 1)
    , testCase "command completion inserts a unique command and a space" $ do
        let prompt = completeCommand ["echo", "exit"] (runText "ec")
        promptText prompt @?= "echo \n"
        cursor prompt @?= (0, 5)
    , testCase "command completion inserts the longest common prefix" $ do
        let prompt = completeCommand ["alpha", "alpine"] (runText "al")
        promptText prompt @?= "alp\n"
        cursor prompt @?= (0, 3)
    , testCase "command completion leaves unknown or non-command words alone" $ do
        promptText (completeCommand ["echo"] (runText "zz")) @?= "zz\n"
        promptText (completeCommand ["echo"] (runText "echo z")) @?= "echo z\n"
    , testCase "command completion suggests commands for an empty word" $ do
        let prompt = completeCommand ["echo"] (runText "")
        promptText prompt @?= "\n"
        promptSuggestions prompt @?= ["echo"]
    , testCase "ambiguous command completion keeps the text and suggests matches" $ do
        let prompt = completeCommand ["echo", "exit"] (runText "e")
        promptText prompt @?= "e\n"
        promptSuggestions prompt @?= ["echo", "exit"]
    , testCase "editing clears completion suggestions" $ do
        let suggested = completeCommand ["echo", "exit"] (runText "e")
            edited = applyKey (KeyChar 'c') suggested
        promptSuggestions edited @?= []
    , testCase "suggestion rows render below the input" $ do
        let prompt = setSuggestions ["echo", "exit"] (runText "e")
        physicalRows 80 "" prompt @?= ["e", "  echo  exit"]
        promptText prompt @?= "e\n"
        suggestionRowCount 80 prompt @?= 1
    , testCase "setPromptText replaces the whole buffer" $ do
        let prompt = setPromptText "one\ntwo" (runText "old")
        lineList prompt @?= ["one", "two"]
        cursor prompt @?= (1, 3)
        promptSuggestions prompt @?= []
    , testCase "needsContinuation handles backslashes and quotes" $ do
        needsContinuation (runText "echo one \\") @?= True
        needsContinuation (runText "echo one") @?= False
        needsContinuation (runText "echo 'unclosed") @?= True
        needsContinuation (runText "echo 'closed'") @?= False
        needsContinuation (runText "echo \"unclosed") @?= True
        needsContinuation (runText "echo \"closed\"") @?= False
        needsContinuation (runText "echo \"escaped \\\" quote\"") @?= False
        needsContinuation (runText "echo 'literal \\'") @?= False
        needsContinuation (run (text "echo 'a" ++ [KeyEnter] ++ text "b'")) @?= False
    , testCase "completionTarget finds the word before the cursor" $ do
        completionTarget (runText "echo fi") @?= Just (5, "fi")
    , testCase "applyCompletion replaces only the target word" $ do
        let prompt = applyCompletion 5 "file" (runText "echo fi")
        promptText prompt @?= "echo file\n"
        cursor prompt @?= (0, 9)
    , testCase "classifyChar maps control keys and printable input" $ do
        classifyChar '\x03' @?= KeyCancel
        classifyChar '\x04' @?= KeySubmit
        classifyChar '\x18' @?= KeyToggleMode
        classifyChar '\n' @?= KeyEnter
        classifyChar '\r' @?= KeyEnter
        classifyChar '\DEL' @?= KeyBackspace
        classifyChar '\b' @?= KeyBackspace
        classifyChar '\t' @?= KeyComplete
        classifyChar 'x' @?= KeyChar 'x'
    , testCase "csiKey maps arrow, home, end, and delete sequences" $ do
        csiKey "" 'A' @?= KeyUp
        csiKey "" 'B' @?= KeyDown
        csiKey "" 'C' @?= KeyRight
        csiKey "" 'D' @?= KeyLeft
        csiKey "" 'H' @?= KeyHome
        csiKey "" 'F' @?= KeyEnd
        csiKey "3" '~' @?= KeyDelete
        csiKey "1;5" 'A' @?= KeyUp
        csiKey "9" '~' @?= KeyIgnore
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "any key sequence keeps the prompt invariants" prop_invariants
    , testProperty "insert followed by backspace restores the prompt" prop_insertBackspace
    , testProperty "Enter followed by backspace restores the prompt" prop_enterBackspace
    , testProperty "Enter, up, end, delete restores the prompt lines" prop_enterDelete
    , testProperty "left then right restores the cursor except at the start" prop_leftRight
    , testProperty "wrapped chunks concatenate back to the line" prop_wrapLine
    , testProperty "physical rows start with the wrapped content" prop_physicalRows
    , testProperty "the cursor is always inside the rendered rows" prop_cursorPosition
    , testProperty "the cursor stays inside rows with a prompt prefix" prop_prefixCursorPosition
    , testProperty "toggleMode is an involution" prop_toggleMode
    ]

-- * Helpers

run :: [Key] -> Prompt
run = foldl (flip applyKey) initialPrompt

runText :: String -> Prompt
runText = run . text

text :: String -> [Key]
text = map KeyChar

cursor :: Prompt -> (Int, Int)
cursor prompt =
  (unRow (promptRow prompt), unColumn (promptColumn prompt))

lineList :: Prompt -> [String]
lineList = NonEmpty.toList . promptLines

invariant :: Prompt -> Bool
invariant prompt =
  unRow (promptRow prompt) >= 0
    && unRow (promptRow prompt) < length (lineList prompt)
    && unColumn (promptColumn prompt) >= 0
    && unColumn (promptColumn prompt) <= length (currentLine prompt)
    && maybe True ((>= 0) . unGoalColumn) (promptGoal prompt)

boundedWidth :: Int -> Int
boundedWidth width = (width `mod` 20 + 20) `mod` 20 + 1

-- * Generators

arbitraryKey :: Gen Key
arbitraryKey =
  oneof
    [ KeyChar <$> elements printableCharacters
    , pure KeyEnter
    , pure KeyBackspace
    , pure KeyDelete
    , pure KeyLeft
    , pure KeyRight
    , pure KeyUp
    , pure KeyDown
    , pure KeyHome
    , pure KeyEnd
    , pure KeySubmit
    , pure KeyCancel
    , pure KeyToggleMode
    , pure KeyComplete
    , pure KeyIgnore
    ]

printableCharacters :: [Char]
printableCharacters = filter (not . isControl) [' ' .. '~']

keySequence :: Gen [Key]
keySequence = listOf arbitraryKey

-- * Properties

prop_invariants :: Property
prop_invariants =
  forAll keySequence $ \keys ->
    let prompt = run keys
     in counterexample (show keys ++ " -> " ++ show prompt) (invariant prompt)

prop_insertBackspace :: Char -> Property
prop_insertBackspace char =
  forAll keySequence $ \keys ->
    let prompt = run keys
        restored = applyKey KeyBackspace (applyKey (KeyChar char) prompt)
     in counterexample (show keys ++ " with " ++ show char ++ " -> " ++ show prompt) $
          lineList restored === lineList prompt
            .&&. promptRow restored === promptRow prompt
            .&&. promptColumn restored === promptColumn prompt

prop_enterBackspace :: Property
prop_enterBackspace =
  forAll keySequence $ \keys ->
    let prompt = run keys
        restored = applyKey KeyBackspace (applyKey KeyEnter prompt)
     in counterexample (show keys ++ " -> " ++ show restored) $
          lineList restored === lineList prompt
            .&&. promptRow restored === promptRow prompt
            .&&. promptColumn restored === promptColumn prompt

prop_enterDelete :: Property
prop_enterDelete =
  forAll keySequence $ \keys ->
    let prompt = run keys
        restored =
          applyKey KeyDelete
            (applyKey KeyEnd (applyKey KeyUp (applyKey KeyEnter prompt)))
     in counterexample (show keys ++ " -> " ++ show restored) $
          lineList restored === lineList prompt
            .&&. promptRow restored === promptRow prompt

prop_leftRight :: Property
prop_leftRight =
  forAll keySequence $ \keys ->
    let prompt = run keys
        restored = applyKey KeyRight (applyKey KeyLeft prompt)
     in counterexample (show keys ++ " -> " ++ show prompt) $
          if unRow (promptRow prompt) == 0 && unColumn (promptColumn prompt) == 0
            then property True
            else cursor restored === cursor prompt

prop_wrapLine :: Int -> String -> Property
prop_wrapLine width line =
  counterexample (show chunks) $
    concat chunks === line
      .&&. property (all ((<= width') . length) chunks)
      .&&. property (not (null chunks))
      .&&. property (null line || not (any null chunks))
  where
    width' = boundedWidth width
    chunks = wrapLine width' line

prop_physicalRows :: Int -> Property
prop_physicalRows width =
  forAll keySequence $ \keys ->
    let width' = boundedWidth width
        prompt = run keys
        content = concatMap (wrapLine width') (lineList prompt)
        rows = physicalRows width' "" prompt
     in counterexample (show rows) $
          take (length content) rows === content
            .&&. property (all null (drop (length content) rows))

prop_cursorPosition :: Int -> Property
prop_cursorPosition width =
  forAll keySequence $ \keys ->
    let width' = boundedWidth width
        prompt = run keys
        (row, column) = cursorPosition width' "" prompt
        rows = physicalRows width' "" prompt
     in counterexample (show prompt ++ " at width " ++ show width') $
          property (row >= 0 && row < length rows)
            .&&. property (column >= 0 && column < width')

prop_prefixCursorPosition :: Int -> Property
prop_prefixCursorPosition width =
  forAll keySequence $ \keys ->
    let width' = boundedWidth width
        prefix = "hutian:/source $ "
        prompt = run keys
        (row, column) = cursorPosition width' prefix prompt
        rows = physicalRows width' prefix prompt
     in counterexample (show prompt ++ " at width " ++ show width') $
          property (row >= 0 && row < length rows)
            .&&. property (column >= 0 && column < width')

prop_toggleMode :: Bool -> Property
prop_toggleMode llmMode =
  toggleMode (toggleMode mode) === mode
  where
    mode = if llmMode then LlmMode else TermMode