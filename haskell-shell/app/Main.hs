{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), IOException, SomeException, catch, finally, fromException, throwIO, try)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Catch as MonadCatch
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Char (isControl, isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
import Data.Word (Word8)
import GHC.IO.Encoding (setLocaleEncoding)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Console.Haskeline
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (splitSearchPath, (</>))
import System.IO
import System.IO.Error (isEOFError)
import System.Posix.IO (stdInput)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)
import System.Posix.Terminal
  ( TerminalAttributes
  , TerminalMode (..)
  , TerminalState (..)
  , getTerminalAttributes
  , setTerminalAttributes
  , withMinInput
  , withMode
  , withTime
  , withoutMode
  )
import System.Process (createProcess, delegate_ctlc, proc, waitForProcess)

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering
  workdir <- fromMaybe "/" <$> lookupEnv "HUTIAN_WORKDIR"
  setCurrentDirectory workdir `catch` \(_ :: IOException) -> pure ()
  interruptRef <- newIORef False
  commands <- executableCommands
  let settings =
        (defaultSettings :: Settings IO)
          { historyFile = Just "/tmp/hutian-shell-history"
          , autoAddHistory = True
          , complete = shellCompletion commands
          }
  runInputT settings (shellLoop interruptRef commands)

shellLoop :: IORef Bool -> [String] -> InputT IO ()
shellLoop interruptRef commands =
  MonadCatch.handle
    ( \exception -> case exception of
        UserInterrupt -> liftIO resetTerminal >> shellLoop interruptRef commands
        other -> MonadCatch.throwM other
    )
    ( handleInterrupt
        (liftIO resetTerminal >> shellLoop interruptRef commands)
        (shellLoopOnce interruptRef commands)
    )

shellLoopOnce :: IORef Bool -> [String] -> InputT IO ()
shellLoopOnce interruptRef commands = do
  cwd <- liftIO getCurrentDirectory
  home <- liftIO (fromMaybe "/" <$> lookupEnv "HOME")
  input <- getInputLine ("hutian:" ++ displayPath home cwd ++ " $ ")
  case input of
    Nothing -> pure ()
    Just line ->
      case words line of
        [] -> shellLoop interruptRef commands
        ("exit" : _) -> pure ()
        ("quit" : _) -> pure ()
        ("help" : _) -> liftIO printHelp >> shellLoop interruptRef commands
        ("pwd" : _) -> liftIO (putStrLn =<< getCurrentDirectory) >> shellLoop interruptRef commands
        ("cd" : args) -> liftIO (changeDirectory args) >> shellLoop interruptRef commands
        ["llm"] -> llmCommand interruptRef >> shellLoop interruptRef commands
        ("llm" : args) -> do
          liftIO (writeIORef interruptRef False)
          liftIO (runLlm interruptRef (unwords args))
          shellLoop interruptRef commands
        _ -> do
          liftIO (writeIORef interruptRef False)
          liftIO (runShellCommand interruptRef line)
          shellLoop interruptRef commands

printHelp :: IO ()
printHelp = do
  putStrLn "Builtins:"
  putStrLn "  cd [dir]   change directory (defaults to $HOME)"
  putStrLn "  pwd        print the current directory"
  putStrLn "  llm        read a prompt until Ctrl+D and stream a llama.cpp response"
  putStrLn "  help       show this help"
  putStrLn "  exit       leave the shell"
  putStrLn "Everything else is executed with /bin/sh."

runShellCommand :: IORef Bool -> String -> IO ()
runShellCommand interruptRef line =
  withInterruptFlag interruptRef $ do
    (_, _, _, processHandle) <-
      createProcess
        (proc "/bin/sh" ["-c", line])
          { delegate_ctlc = False
          }
    _ <- waitForProcess processHandle
    pure ()

withInterruptFlag :: IORef Bool -> IO a -> IO a
withInterruptFlag interruptRef action = do
  previousHandler <- installHandler sigINT (Catch (writeIORef interruptRef True)) Nothing
  action `finally` installHandler sigINT previousHandler Nothing

changeDirectory :: [String] -> IO ()
changeDirectory args = do
  home <- fromMaybe "/" <$> lookupEnv "HOME"
  let target = expandHome home (if null args then home else unwords args)
  result <- try (setCurrentDirectory target) :: IO (Either IOException ())
  case result of
    Left err -> hPutStrLn stderr ("cd: " ++ show err)
    Right () -> pure ()

expandHome :: FilePath -> FilePath -> FilePath
expandHome home target
  | target == "~" = home
  | "~/" `isPrefixOf` target = home </> drop 2 target
  | otherwise = target

displayPath :: FilePath -> FilePath -> FilePath
displayPath home cwd =
  case stripPrefix home cwd of
    Just "" -> "~"
    Just rest -> "~" ++ rest
    Nothing -> cwd

llmCommand :: IORef Bool -> InputT IO ()
llmCommand interruptRef = do
  result <-
    liftIO (withRawTerminal (readMultiline []))
      `MonadCatch.onException` liftIO resetTerminal
  case result of
    Nothing -> pure ()
    Just prompt -> do
      liftIO (writeIORef interruptRef False)
      liftIO (runLlm interruptRef prompt)

withRawTerminal :: IO a -> IO a
withRawTerminal action = do
  isTerminal <- hIsTerminalDevice stdin
  if not isTerminal
    then action
    else do
      oldAttributes <- getTerminalAttributes stdInput
      setTerminalAttributes stdInput (rawAttributes oldAttributes) Immediately
      action `finally` setTerminalAttributes stdInput oldAttributes Immediately

rawAttributes :: TerminalAttributes -> TerminalAttributes
rawAttributes oldAttributes =
  let withoutInputModes =
        foldl withoutMode oldAttributes
          [ ProcessInput
          , EnableEcho
          , EchoErase
          , EchoKill
          , KeyboardInterrupts
          , ExtendedFunctions
          ]
      withOutputModes =
        foldl withMode withoutInputModes
          [ ProcessOutput
          , MapLFtoCRLF
          ]
   in withTime (withMinInput withOutputModes 1) 0

readMultiline :: [String] -> IO (Maybe String)
readMultiline linesDone = do
  startLlmLine
  go linesDone ""
  where
    go completed current = do
      result <- try (hGetChar stdin) :: IO (Either IOException Char)
      case result of
        Left err
          | isEOFError err -> finishInput completed current
          | otherwise -> ioError err
        Right char -> case char of
          '\x04' -> finishInput completed current
          '\x03' -> cancelInput completed current
          '\r' -> nextLine completed current
          '\n' -> nextLine completed current
          '\DEL' -> backspace completed current
          '\b' -> backspace completed current
          '\ESC' -> skipEscape >> go completed current
          _
            | isControl char -> go completed current
            | otherwise -> do
                putChar char
                hFlush stdout
                go completed (current ++ [char])

    nextLine completed current = do
      putStr "\n"
      hFlush stdout
      startLlmLine
      go (current : completed) ""

    backspace completed current =
      case reverse current of
        [] -> go completed current
        _ : rest -> do
          putStr "\b \b"
          hFlush stdout
          go completed (reverse rest)

    finishInput completed current = do
      finishLlmInput (not (null current))
      pure (Just (unlines (reverse (includeCurrent current completed))))

    cancelInput _ current = do
      finishLlmInput (not (null current))
      pure Nothing

skipEscape :: IO ()
skipEscape = do
  next <- try (hGetChar stdin) :: IO (Either IOException Char)
  case next of
    Right '[' -> skipUntilFinal
    Right 'O' -> skipUntilFinal
    _ -> pure ()
  where
    skipUntilFinal = do
      next <- try (hGetChar stdin) :: IO (Either IOException Char)
      case next of
        Right char
          | char >= '@' && char <= '~' -> pure ()
          | otherwise -> skipUntilFinal
        Left _ -> pure ()

includeCurrent :: String -> [String] -> [String]
includeCurrent current completed
  | null current = completed
  | otherwise = current : completed

startLlmLine :: IO ()
startLlmLine = do
  isTerminal <- hIsTerminalDevice stdout
  when isTerminal (putStr llmBackground >> hFlush stdout)

llmBackground :: String
llmBackground = "\ESC[48;2;40;0;40m\ESC[K"

finishLlmInput :: Bool -> IO ()
finishLlmInput hasPartial = do
  isTerminal <- hIsTerminalDevice stdout
  when isTerminal $ do
    when hasPartial (putStr "\n")
    putStr "\ESC[0m"
  hFlush stdout

resetTerminal :: IO ()
resetTerminal = do
  isTerminal <- hIsTerminalDevice stdout
  when isTerminal (putStr "\ESC[0m" >> hFlush stdout)

runLlm :: IORef Bool -> String -> IO ()
runLlm interruptRef prompt
  | all isSpace prompt = pure ()
  | otherwise = do
      url <- fromMaybe "http://127.0.0.1:8080/v1/chat/completions" <$> lookupEnv "HUTIAN_LLAMA_URL"
      model <- fromMaybe "default" <$> lookupEnv "HUTIAN_LLAMA_MODEL"
      result <- try (withInterruptFlag interruptRef (streamCompletion interruptRef url model prompt)) :: IO (Either SomeException ())
      case result of
        Left err ->
          case fromException err :: Maybe Interrupt of
            Just _ -> throwIO err
            Nothing -> hPutStrLn stderr ("llm: " ++ show err)
        Right () -> pure ()

streamCompletion :: IORef Bool -> String -> String -> String -> IO ()
streamCompletion interruptRef url model prompt = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody =
              RequestBodyLBS
                ( Aeson.encode
                    ( if "/v1/chat/completions" `isInfixOf` url
                        then
                          object
                            [ "model" .= model
                            , "messages"
                                .= [object ["role" .= ("user" :: String), "content" .= prompt]]
                            , "stream" .= True
                            ]
                        else
                          object
                            [ "model" .= model
                            , "prompt" .= prompt
                            , "stream" .= True
                            ]
                    )
                )
          , responseTimeout = responseTimeoutNone
          }
  withResponse request manager $ \response -> do
    let code = statusCode (responseStatus response)
    if code < 200 || code >= 300
      then do
        body <- readBody (responseBody response) []
        let message = Text.unpack (Text.strip (decode body))
        hPutStrLn stderr ("llm: llama.cpp returned HTTP " ++ show code ++ errorSuffix message)
      else do
        (printed, sawData) <- consumeResponse interruptRef (responseBody response) BS.empty False False
        when printed (putStrLn "")
        unless sawData (hPutStrLn stderr "llm: no response content received")

readBody :: BodyReader -> [BS.ByteString] -> IO BS.ByteString
readBody reader chunks = do
  chunk <- brRead reader
  if BS.null chunk
    then pure (BS.concat (reverse chunks))
    else readBody reader (chunk : chunks)

consumeResponse :: IORef Bool -> BodyReader -> BS.ByteString -> Bool -> Bool -> IO (Bool, Bool)
consumeResponse interruptRef reader buffered printed sawData = do
  interrupted <- readIORef interruptRef
  if interrupted
    then pure (printed, sawData)
    else do
      chunk <- brRead reader
      if BS.null chunk
        then do
          (printed', sawData', _) <- processLines (fst (splitLines buffered)) printed sawData
          pure (printed', sawData')
        else do
          let (lineChunks, rest) = splitLines (buffered <> chunk)
          (printed', sawData', shouldStop) <- processLines lineChunks printed sawData
          if shouldStop
            then pure (printed', sawData')
            else consumeResponse interruptRef reader rest printed' sawData'

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines = go []
  where
    go acc input =
      case BS.elemIndex 10 input of
        Nothing -> (reverse acc, input)
        Just index ->
          let (line, rest) = BS.splitAt index input
           in go (line : acc) (BS.drop 1 rest)

processLines :: [BS.ByteString] -> Bool -> Bool -> IO (Bool, Bool, Bool)
processLines [] printed sawData = pure (printed, sawData, False)
processLines (line : lines') printed sawData = do
  (printed', sawData', shouldStop) <- processLine line printed sawData
  if shouldStop
    then pure (printed', sawData', True)
    else processLines lines' printed' sawData'

processLine :: BS.ByteString -> Bool -> Bool -> IO (Bool, Bool, Bool)
processLine rawLine printed sawData
  | BS.null line = pure (printed, sawData, False)
  | "data:" `BS.isPrefixOf` line = do
      let payload = Text.strip (decode (BS.drop 5 line))
      if payload == "[DONE]"
        then pure (printed, True, True)
        else processJson payload printed True
  | otherwise = processJson (Text.strip (decode line)) printed sawData
  where
    line = BS.dropWhileEnd isCarriageReturn rawLine

isCarriageReturn :: Word8 -> Bool
isCarriageReturn = (== 13)

processJson :: Text.Text -> Bool -> Bool -> IO (Bool, Bool, Bool)
processJson payload printed sawData =
  case Aeson.decodeStrict' (TextEncoding.encodeUtf8 payload) of
    Just (Object value) -> do
      let content = extractContent value
      case content of
        Just text -> do
          putStr (Text.unpack text)
          hFlush stdout
        Nothing -> pure ()
      case extractError value of
        Just message -> hPutStrLn stderr ("llm: " ++ Text.unpack message)
        Nothing -> pure ()
      let printed' = printed || maybe False (not . Text.null) content
      pure (printed', True, isStop value)
    _ -> pure (printed, sawData, False)

extractContent :: KeyMap.KeyMap Value -> Maybe Text.Text
extractContent value =
  case KeyMap.lookup "content" value of
    Just (String text) -> Just text
    _ ->
      case KeyMap.lookup "choices" value of
        Just (Array choices) -> listToMaybe (mapMaybe choiceContent (Vector.toList choices))
        _ -> Nothing
  where
    choiceContent (Object choice) =
      case KeyMap.lookup "delta" choice of
        Just (Object delta) ->
          case KeyMap.lookup "content" delta of
            Just (String text) -> Just text
            _ -> Nothing
        _ -> Nothing
    choiceContent _ = Nothing

extractError :: KeyMap.KeyMap Value -> Maybe Text.Text
extractError value =
  case KeyMap.lookup "error" value of
    Just (String message) -> Just message
    Just (Object nested) ->
      case KeyMap.lookup "message" nested of
        Just (String message) -> Just message
        _ -> Just (Text.pack (show (Object nested)))
    _ -> Nothing

isStop :: KeyMap.KeyMap Value -> Bool
isStop value =
  case KeyMap.lookup "stop" value of
    Just (Bool True) -> True
    _ ->
      case KeyMap.lookup "choices" value of
        Just (Array choices) -> any choiceFinished (Vector.toList choices)
        _ -> False
  where
    choiceFinished (Object choice) =
      case KeyMap.lookup "finish_reason" choice of
        Nothing -> False
        Just Null -> False
        Just _ -> True
    choiceFinished _ = False

decode :: BS.ByteString -> Text.Text
decode = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

errorSuffix :: String -> String
errorSuffix "" = ""
errorSuffix message = ": " ++ message

executableCommands :: IO [String]
executableCommands = do
  searchPath <- fromMaybe "" <$> lookupEnv "PATH"
  names <- fmap concat $ forM (splitSearchPath searchPath) $ \dir -> do
    entries <- ignoringIOError [] (listDirectory dir)
    fmap concat $ forM entries $ \entry -> do
      let candidate = dir </> entry
      isDirectory <- ignoringIOError False (doesDirectoryExist candidate)
      permissions <- ignoringIOError emptyPermissions (getPermissions candidate)
      pure $
        if not isDirectory && executable permissions
          then [entry]
          else []
  pure . Set.toList . Set.fromList $ names ++ builtins
  where
    builtins = ["cd", "exit", "help", "llm", "pwd", "quit"]

ignoringIOError :: a -> IO a -> IO a
ignoringIOError fallback action = action `catch` \(_ :: IOException) -> pure fallback

shellCompletion :: [String] -> CompletionFunc IO
shellCompletion commands (reversedLeft, right)
  | all isSpace beforeWord && '/' `notElem` word =
      case filter (word `isPrefixOf`) commands of
        [] -> completeFilename (reversedLeft, right)
        matches -> pure (beforeWord, [Completion match match True | match <- matches])
  | otherwise = completeFilename (reversedLeft, right)
  where
    (reversedWord, beforeWord) = break isSpace reversedLeft
    word = reverse reversedWord
