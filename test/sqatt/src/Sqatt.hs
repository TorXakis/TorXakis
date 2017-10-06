{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Integration test utilities.
module Sqatt
  ( TxsExample(..)
  , checkSMTSolvers
  , checkCompilers
  , checkTxsInstall
  , ExampleResult (..)
  , javaCmd
  , TxsExampleSet (..)
  , SutExample (..)
  , toFSSafeStr
  -- * Testing
  , testExamples
  , testExampleSet
  , testExampleSets
  -- * Benchmarking
  , benchmarkExampleSet
  -- * Logging
  , sqattLogsRoot
  , mkLogDir
  -- * Re-exports
  , module Turtle
  )
where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Foldl
import           Control.Monad.Except
import           Control.Monad.Extra
import           Criterion.Main
import           Data.Either
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           System.Info
import           System.Random
import           Test.Hspec
import           Turtle

-- * Data structures for specifying examples

-- | A description of a TorXakis example.
data TxsExample
  = TxsExample {
    -- | Name of the example.
    exampleName    :: String
    -- | Paths to the TorXakis model files.
  , txsModelFiles  :: [FilePath]
    -- | Paths to the files containing the commands that will be passed to the
    --   TorXakis server. Commands are passed in the order specified by the
    --   order of the files in the list.
  , txsCmdsFiles   :: [FilePath]
    -- | Command line arguments to be passed to the TorXakis server command.
  , txsServerArgs  :: [Text]
    -- | SUT example. This run together with TorXakis. If this field is
    --   `Nothing` then the example is assumed to be autonomous (only TorXakis
    --   will be run)
  , sutExample     :: Maybe SutExample
    -- | Example's expected result.
  , expectedResult :: ExampleResult
  } deriving (Show)

data SutExample
  -- | A Java SUT that must be compiled and executed.
  = JavaExample {
    -- | Source file of the SUT.
    javaSourcePath :: FilePath
    -- | Arguments to be passed to the SUT.
  , javaSutArgs    :: [Text]
  }
  -- | A TorXakis simulated SUT. The FilePath specifies the location of the
  -- commands to be input to the simulator.
  | TxsSimulator FilePath
  deriving (Show)

-- | A set of examples.
data TxsExampleSet
  = TxsExampleSet {
    -- | Description of the example set.
    exampleSetdesc :: ExampleSetDesc
    -- | Examples in the set.
  , txsExamples    :: [TxsExample]
  }

-- | Description of the example set.
newtype ExampleSetDesc
  = ExampleSetDesc {
    -- | Name of the example set.
    exampleSetName :: String
  }

instance IsString ExampleSetDesc where
  fromString = ExampleSetDesc

-- | Information about a compiled Java program.
data CompiledSut
  -- | `JavaCompiledSut mainClass mClassSP`:
  --
  --   - `mainClass`: name of the main Java class.
  --
  --   - `mClassSP`: Class search path. If omitted no `-cp` option will be
  --     passed to the `java` command.
  --
  = JavaCompiledSut Text (Maybe FilePath)
  -- | An SUT simulated by TorXakis.
  --
  --  `TxsSimulatedSut modelPaths cmds`:
  --
  --   - `modelPaths`: Paths to the TorXakis models.
  --   - `cmds`: Commands to be passed to the simulator.
  | TxsSimulatedSut [FilePath] FilePath

-- | A processed example, ready to be run.
--
-- Currently the only processing that takes place is the compilation of the
-- SUT, if any.
data RunnableExample = ExampleWithSut TxsExample CompiledSut [Text]
                     | StandaloneExample TxsExample

data ExampleResult = Pass | Fail | Message Text deriving (Show, Eq)

-- * Path manipulation functions

addExeSuffix :: Text -> Text
addExeSuffix path = if os == "mingw32" then path <> ".exe" else path

javaCmd :: Text
javaCmd = addExeSuffix "java"

javacCmd :: Text
javacCmd = addExeSuffix "javac"

txsServerCmd :: Text
txsServerCmd = addExeSuffix "txsserver"

txsUICmd :: Text
txsUICmd = addExeSuffix "txsui"

txsUILinePrefix :: Text
txsUILinePrefix = "TXS >>  "


class ExpectedMessage a where
    expectedMessage :: a -> Text

instance ExpectedMessage ExampleResult where
    expectedMessage Pass          = txsUILinePrefix <> "PASS"
    expectedMessage Fail          = txsUILinePrefix <> "FAIL"
    expectedMessage (Message msg) = txsUILinePrefix <> msg

-- | Decode a file path into a human readable text string. The decoding is
-- dependent on the operating system. An error is thrown if the decoding is not
-- successful.
decodePath :: FilePath -> Test Text
decodePath filePath =
  case toText filePath of
    Right path ->
      return path
    Left apprPath ->
      throwError $ FilePathError $
        "Cannot decode " <> apprPath <> " properly"

-- | Replace the characters that might cause problems on Windows systems.
toFSSafeStr :: String -> String
toFSSafeStr str = repl <$> str
  where repl ' ' = '_'
        repl ':' = '-'
        repl c   = c

-- * Environment checking

-- | Check that all the supported SMT solvers are installed.
--
-- Throws an exception on failure.
checkSMTSolvers :: IO ()
checkSMTSolvers = do
  putStrLn "WARNING: The presence of SMT solvers was not checked."
  putStrLn "         First issue #47 needs to be resolved."
  putStrLn "See: https://github.com/TorXakis/TorXakis/issues/47"


-- | Check that the given command exists in the search path of the host system.
checkCommand :: Text -> IO ()
checkCommand cmd = do
  path <- which (fromText cmd)
  case path of
    Nothing -> throwIO $ ProgramNotFound (T.pack (show cmd))
    _       -> return ()

-- | Check that all the compilers are installed.
--
-- Throws an exception on failure.
checkCompilers :: IO ()
checkCompilers = traverse_ checkCommand [javaCmd, javacCmd]

-- | Check that the TorXakis UI and server programs are installed.
checkTxsInstall :: IO ()
checkTxsInstall = traverse_ checkCommand [txsUICmd, txsServerCmd]

-- * Compilation and testing

-- | Sqatt test monad.
newtype Test a = Test { runTest :: ExceptT SqattError IO a }
  deriving (Functor, Monad, Applicative, MonadError SqattError, MonadIO)

-- | Test errors that can arise when running a TorXakis example.
data SqattError = CompileError Text
                | ProgramNotFound Text
                | UnsupportedLanguage Text
                | FilePathError Text
                | TestExpectationError Text
                | SutAborted
                | TxsServerAborted
                | TestTimedOut
  deriving (Show, Eq)

instance Exception SqattError

-- | Compile the system under test.
compileSut :: FilePath -> Test CompiledSut
compileSut sourcePath =
  case extension sourcePath of
    Just "java" ->
      compileJavaSut sourcePath
    _    -> do
      path <- decodePath sourcePath
      throwError $ UnsupportedLanguage $
        "Compiler not found for file " <> path

-- | Compile a SUT written in Java.
compileJavaSut :: FilePath -> Test CompiledSut
compileJavaSut sourcePath = do
  path <- decodePath sourcePath
  exitCode <- proc javacCmd [path] mempty
  case exitCode of
    ExitFailure code ->
      throwError $ CompileError $
        "Java compilation command failed with code: " <> (T.pack . show) code
    ExitSuccess -> do
      mClass <- decodePath $ basename sourcePath
      let sPath = directory sourcePath
      return $ JavaCompiledSut mClass (Just sPath)

-- | Add the class path option if a class-path is given.
getCPOptsIO :: Maybe FilePath -> IO [Text]
getCPOptsIO Nothing         = return []
getCPOptsIO (Just filePath) = case toText filePath of
                                Left apprPath ->
                                  throw $ FilePathError $
                                  "Cannot decode " <> apprPath <> " properly"
                                Right path ->
                                  return ["-cp", path]

-- | Timeout (in seconds) for running a test. For now the timeout is not
-- configurable.
sqattTimeout :: NominalDiffTime
sqattTimeout = 1800.0

-- | Time to allow TorXakis run the checks after the SUT terminates. After this
-- timeout the SUT process terminates and if the expected result is not
-- observed in the test the whole test fails.
txsCheckTimeout :: NominalDiffTime
txsCheckTimeout = 60.0

-- | Run TorXakis with the given example specification.
runTxsWithExample :: Maybe FilePath   -- ^ Path to the logging directory for
                                      -- the current example set, or nothing if
                                      -- no logging is desired.
                  -> TxsExample       -- ^ Example to run.
                  -> Concurrently (Either SqattError ())
runTxsWithExample mLogDir ex = Concurrently $ do
  eInputModelF <- runExceptT $ runTest $ mapM decodePath (txsModelFiles ex)

  case eInputModelF of
    Left decodeErr -> return $ Left decodeErr
    Right inputModelF -> do
      port <- repr <$> getRandomPort
      runConcurrently $ timer
                    <|> heartbeat
                    <|> txsServerProc mLogDir (port : txsServerArgs ex)
                    <|> txsUIProc mLogDir inputModelF port
  where
    heartbeat = Concurrently $ forever $ do
      sleep 60.0 -- For now we don't make this configurable.
      putStr "."
    timer = Concurrently $ do
      sleep sqattTimeout
      throwIO TestTimedOut
    txsUIProc mUiLogDir imf port =
      Concurrently $ try $ do
        res <- Turtle.fold txsUIShell findExpectedMsg
        unless res (throw tErr)
      where
        inLines :: Shell Line
        inLines = asum $ input <$> cmdsFile
        txsUIShell :: Shell Line
        txsUIShell =
            case mUiLogDir of
                Nothing ->
                    either id id <$> inprocWithErr txsUICmd
                                                        (port:imf)
                                                        inLines
                Just uiLogDir -> do
                    h <- appendonly $ uiLogDir </> "txsui.out.log"
                    line <- either id id <$> inprocWithErr txsUICmd
                                                           (port:imf)
                                                           inLines
                    liftIO $ TIO.hPutStrLn h (lineToText line)
                    return line
        findExpectedMsg :: Fold Line Bool
        findExpectedMsg = Control.Foldl.any (T.isInfixOf searchStr . lineToText)
    cmdsFile = txsCmdsFiles ex
    searchStr = expectedMessage . expectedResult $ ex
    tErr = TestExpectationError $
              format ("Did not get expected result "%s)
                     (repr . expectedResult $ ex)
    txsServerProc sLogDir args = Concurrently $
      runInprocNI ((</> "txsserver.out.log") <$> sLogDir) txsServerCmd args

-- | Run a process.
runInproc :: Maybe FilePath   -- ^ Directory where the logs will be stored, or @Nothing@ if no logging is desired.
          -> Text             -- ^ Command to run.
          -> [Text]           -- ^ Command arguments.
          -> Shell Line       -- ^ Lines to be input to the command.
          -> IO (Either SqattError ())
runInproc mLogDir cmd cmdArgs procInput =
    case mLogDir of
        Nothing ->
            try $ sh $ inprocWithErr cmd cmdArgs procInput
        Just logDir ->
            try $ output logDir $
                either id id <$> inprocWithErr cmd cmdArgs procInput

-- | Run a process without input. See `runInproc`.
--
runInprocNI :: Maybe FilePath
            -> Text
            -> [Text]
            -> IO (Either SqattError ())
runInprocNI mLogDir cmd cmdArgs =
  runInproc mLogDir cmd cmdArgs Turtle.empty

-- | Run TorXakis as system under test.
runTxsAsSut :: Maybe FilePath   -- ^ Path to the logging directory for the current
                                -- example set, or @Nothing@ if no logging is desired.
            -> [FilePath]       -- ^ List of paths to the TorXakis model.
            -> FilePath         -- ^ Path to the commands to be input to the TorXakis model.
            -> IO (Either SqattError ())
runTxsAsSut mLogDir modelFiles cmdsFile = do
  eInputModelF <- runExceptT $ runTest $ mapM decodePath modelFiles
  case eInputModelF of
    Left decodeErr -> return $ Left decodeErr
    Right inputModelF -> do
      port <- repr <$> getRandomPort
      runConcurrently $
        txsServerProc port <|> txsUIProc inputModelF port
  where
    txsUIProc imf port = Concurrently $
      let mCLogDir = (</> "txsui.SUT.out.log") <$> mLogDir in
      runInproc mCLogDir txsUICmd (port:imf) (input cmdsFile)
    txsServerProc port = Concurrently $
      let mCLogDir = (</> "txsserver.SUT.out.log") <$> mLogDir in
      runInprocNI mCLogDir txsServerCmd [port]

mkTest :: Maybe FilePath -> RunnableExample -> Test ()
mkTest mLogDir (ExampleWithSut ex cSUT args) = do
  res <- liftIO $
    runConcurrently $  runSUTWithTimeout mLogDir cSUT args
                   <|> runTxsWithExample mLogDir ex
  case res of
    Left txsErr -> throwError txsErr
    Right ()    -> return ()
mkTest mLogDir (StandaloneExample ex) = do
  res <- liftIO $ runConcurrently $ runTxsWithExample mLogDir ex
  case res of
    Left txsErr -> throwError txsErr
    Right  _    -> return ()

runSUTWithTimeout :: Maybe FilePath -> CompiledSut -> [Text]
                  -> Concurrently (Either SqattError ())
runSUTWithTimeout mLogDir cSUT args = Concurrently $ do
  res <- runSUT mLogDir cSUT args
  case res of
    Left someErr ->
      return $ Left someErr
    Right _ -> do
      sleep txsCheckTimeout
      return (Left TestTimedOut)

runSUT :: Maybe FilePath -> CompiledSut -> [Text]
       -> IO (Either SqattError ())
runSUT mLogDir (JavaCompiledSut mClass cpSP) args = do
  cpOpts <- getCPOptsIO cpSP
  let javaArgs = cpOpts ++ [mClass] ++ args
  runInprocNI ((</> "SUT.out.log") <$> mLogDir) javaCmd javaArgs
runSUT logDir (TxsSimulatedSut modelFiles cmds) _ =
  runTxsAsSut logDir modelFiles cmds

-- | Get a random port number.
getRandomPort :: IO Integer
getRandomPort = randomRIO (10000, 60000)

-- | Check that the file exists.
pathMustExist :: FilePath -> Test ()
pathMustExist path =
  unlessM (testpath path) (throwError sqattErr)
  where sqattErr =
          FilePathError $ format ("file "%s%" does not exists ") (repr path)

-- | Retrieve all the file paths from an example
exampleInputFiles :: TxsExample -> [FilePath]
exampleInputFiles ex =
  (txsCmdsFiles ex ++ txsModelFiles ex)
  ++ fromMaybe [] (sutInputFiles <$> sutExample ex)
  where sutInputFiles (JavaExample jsp _)     = [jsp]
        sutInputFiles (TxsSimulator cmdsFile) = [cmdsFile]

-- | Execute a test.
execTest :: Maybe FilePath -> TxsExample -> Test ()
execTest mTopLogDir ex = do
  let mLogDir = (</> (fromString . toFSSafeStr . exampleName) ex) <$> mTopLogDir
  for_ mLogDir mktree
  traverse_ pathMustExist (exampleInputFiles ex)
  runnableExample <- getRunnableExample
  mkTest mLogDir runnableExample
  where
    getRunnableExample =
      case sutExample ex of
        Nothing ->
          return (StandaloneExample ex)
        Just (JavaExample sourcePath args) -> do
          cmpSut <- compileSut sourcePath
          return (ExampleWithSut ex cmpSut args)
        Just (TxsSimulator cmds) ->
          return (ExampleWithSut ex (TxsSimulatedSut (txsModelFiles ex) cmds) [])

-- | Test a single example.
testExample :: FilePath -> TxsExample -> Spec
testExample logDir ex = it (exampleName ex) $ do
  res <- runExceptT $ runTest $ execTest (Just logDir) ex
  res `shouldBe` Right ()

-- | Test a list of examples.
testExamples :: FilePath -> [TxsExample] -> Spec
testExamples logDir = traverse_ (testExample logDir)

-- | Make a benchmark from a TorXakis example.
mkBenchmark :: TxsExample -> Benchmark
mkBenchmark ex =
    bench (exampleName ex) $ nfIO runBenchmark
    where
      runBenchmark = do
          res <- runExceptT $ runTest $ execTest Nothing ex
          unless (isRight res) (error $ "Unexpected error: " ++ show res)

-- | Make a list of benchmarks from a list of TorXakis examples.
mkBenchmarks :: [TxsExample] -> [Benchmark]
mkBenchmarks = (mkBenchmark <$>)

-- | Run benchmarks on a set of examples.
benchmarkExampleSet :: TxsExampleSet -> Benchmark
benchmarkExampleSet (TxsExampleSet exSetDesc exs) =
    bgroup groupName benchmarks
    where
      groupName = exampleSetName exSetDesc
      benchmarks = mkBenchmarks exs

esLogDir :: FilePath -> TxsExampleSet -> FilePath
esLogDir logRoot exSet =
    logRoot </> (  fromString
                . toFSSafeStr
                . exampleSetName
                . exampleSetdesc) exSet

-- | Test an example set.
testExampleSet :: FilePath -> TxsExampleSet -> Spec
testExampleSet logDir es@(TxsExampleSet exSetDesc exs) = do
  let thisSetLogDir = esLogDir logDir es
  runIO $ mktree thisSetLogDir
  describe (exampleSetName exSetDesc) (testExamples thisSetLogDir exs)

-- | Test a list of example sets.
testExampleSets :: FilePath -> [TxsExampleSet] -> Spec
testExampleSets logDir = traverse_ (testExampleSet logDir)

-- | For now the root directory where the logs are stored is not configurable.
sqattLogsRoot :: FilePath
sqattLogsRoot = "sqatt-logs"

-- | Create a log directory with the specified prefix.
mkLogDir :: String -> IO FilePath
mkLogDir strPrefix = do
    currDate <- date
    let logDir =
            sqattLogsRoot </> fromString (strPrefix ++ currDateStr)
        currDateStr = toFSSafeStr (show currDate)
    mktree logDir
    return logDir
