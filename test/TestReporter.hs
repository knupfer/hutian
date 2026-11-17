{-# LANGUAGE ImplicitParams #-}

-- | A Tasty runner that renders the report only after every test has finished.
--
-- Several tests redirect the process-wide stdout and stderr to observe what the
-- shell writes (see "TestSupport"). Tasty's console reporter prints test names
-- and results while the tests are still running, so those writes can end up in
-- a capture and make otherwise deterministic tests fail. Deferring the whole
-- report until the status map is done keeps reporter output and test output
-- apart, at the price of no live progress.
module TestReporter (bufferedMain) where

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, retry)
import Control.Monad (unless, void, when)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Monoid (Any (..))
import Data.Proxy (Proxy (..))
import System.Console.ANSI (hSupportsANSI, hSupportsANSIColor)
import System.IO (BufferMode (LineBuffering), hFlush, hSetBuffering, stdout)
import Test.Tasty (TestTree, defaultMainWithIngredients)
import Test.Tasty.Ingredients.ConsoleReporter
  ( AnsiTricks (..)
  , HideSuccesses (..)
  , MinDurationToReport (..)
  , Quiet (..)
  , Statistics (..)
  , TestOutput
  , UseColor (..)
  , buildTestOutput
  , computeStatistics
  , foldTestOutput
  , printStatistics
  , useColor
  )
import Test.Tasty.Options
  ( OptionDescription (Option)
  , OptionSet
  , changeOption
  , lookupOption
  )
import Test.Tasty.Runners
  ( Ap (..)
  , Ingredient (..)
  , Result
  , Status (..)
  , StatusMap
  , Time
  , Traversal (..)
  , listingTests
  , resultSuccessful
  )

bufferedMain :: TestTree -> IO ()
bufferedMain = defaultMainWithIngredients [listingTests, bufferedReporter]

-- | Console reporter that prints nothing while tests are running.
bufferedReporter :: Ingredient
bufferedReporter =
  TestReporter
    [ Option (Proxy :: Proxy Quiet)
    , Option (Proxy :: Proxy HideSuccesses)
    , Option (Proxy :: Proxy MinDurationToReport)
    , Option (Proxy :: Proxy UseColor)
    , Option (Proxy :: Proxy AnsiTricks)
    ]
    $ \opts tree -> Just $ \smap ->
      case lookupOption opts of
        Quiet True -> do
          succeeded <- waitForQuietResult smap
          pure (\_ -> pure succeeded)
        Quiet False -> verboseReport opts tree smap

-- | Render the full report once no test is running anymore.
verboseReport :: OptionSet -> TestTree -> StatusMap -> IO (Time -> IO Bool)
verboseReport opts tree smap = do
  isTerminal <- hSupportsANSI stdout
  isTerminalColor <- hSupportsANSIColor stdout
  let ?colors = useColor (lookupOption opts) isTerminalColor
  let HideSuccesses hideSuccesses = lookupOption opts
      -- 'buildTestOutput' is pure and cannot query the terminal itself.
      opts' =
        changeOption
          (\(AnsiTricks tricks) -> AnsiTricks (tricks && isTerminal))
          opts
      toutput = buildTestOutput opts' tree
  hSetBuffering stdout LineBuffering
  -- No test may still be redirecting stdout or stderr from here on.
  waitForAllTests smap
  if hideSuccesses
    then renderFailuresOnly toutput smap
    else renderAll toutput smap
  hFlush stdout
  pure $ \time -> do
    stats <- computeStatistics smap
    printStatistics stats time
    pure (statFailures stats == 0)

-- | Block until every test has a final result. Skipped tests are recorded as
-- 'Done' too, so this terminates even when a dependency failed.
waitForAllTests :: StatusMap -> IO ()
waitForAllTests smap = atomically $ mapM_ waitDone (IntMap.elems smap)
  where
    waitDone :: TVar Status -> STM ()
    waitDone status = do
      current <- readTVar status
      case current of
        Done _ -> pure ()
        _ -> retry

-- | Fail fast, like Tasty's quiet reporter: finish as soon as a test fails,
-- otherwise once every test succeeded.
waitForQuietResult :: StatusMap -> IO Bool
waitForQuietResult smap = atomically go
  where
    go :: STM Bool
    go = do
      statuses <- traverse readTVar (IntMap.elems smap)
      if any isFailure statuses
        then pure False
        else
          if all isDone statuses
            then pure True
            else retry
    isFailure (Done result) = not (resultSuccessful result)
    isFailure _ = False
    isDone Done {} = True
    isDone _ = False

-- | Equivalent to Tasty's non-hiding console output.
renderAll :: TestOutput -> StatusMap -> IO ()
renderAll toutput smap =
  getTraversal . fst $ foldTestOutput foldTest foldHeading toutput smap
  where
    foldTest
      :: String
      -> IO ()
      -> IO Result
      -> (Result -> IO ())
      -> (Traversal IO, Any)
    foldTest _ printName getResult printResult =
      (Traversal $ do
         printName
         result <- getResult
         printResult result
      , Any True)
    foldHeading
      :: String
      -> IO ()
      -> (Traversal IO, Any)
      -> (Traversal IO, Any)
    foldHeading _ printHeading (printBody, Any nonempty) =
      ( Traversal $
          when nonempty $ do
            printHeading
            getTraversal printBody
      , Any nonempty
      )

-- | Only print the tests that failed, keeping their group headings.
renderFailuresOnly :: TestOutput -> StatusMap -> IO ()
renderFailuresOnly toutput smap = do
  headings <- newIORef []
  void . getApp $ foldTestOutput (foldTest headings) (foldHeading headings) toutput smap
  where
    foldTest
      :: IORef [IO ()]
      -> String
      -> IO ()
      -> IO Result
      -> (Result -> IO ())
      -> Ap IO Any
    foldTest headings _ printName getResult printResult = Ap $ do
      result <- getResult
      if resultSuccessful result
        then pure (Any False)
        else do
          pending <- reverse <$> readIORef headings
          writeIORef headings []
          sequence_ pending
          printName
          printResult result
          pure (Any True)
    foldHeading :: IORef [IO ()] -> String -> IO () -> Ap IO Any -> Ap IO Any
    foldHeading headings _ printHeading printBody = Ap $ do
      modifyIORef' headings (printHeading :)
      Any failed <- getApp printBody
      unless failed $ modifyIORef' headings (drop 1)
      pure (Any failed)
