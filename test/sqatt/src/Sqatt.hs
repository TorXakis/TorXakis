{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Integration test utilities.
module Sqatt
  ( TxsExample(..)
  , checkSMTSolvers
  , checkCompilers
  , testExamples
  , checkTxsInstall
  , ExampleResult (..)
  , javaCmd
  , testExampleSet
  , testExampleSets
  , TxsExampleSet (..)
  )
where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Foldl
import           Control.Monad.Except
import           Data.Either
import           Data.Foldable
import           Data.Monoid
import           Data.Text
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Network.Socket
import           Prelude                   hiding (FilePath)
import           System.Info
import qualified System.IO                 as IO
import           System.IO.Silently
import           Test.Hspec
import           Turtle

-- * Data structures for specifying examples

-- | A description of a TorXakis example.
data TxsExample = TxsExample
  { exampleName     :: String         -- ^ Name of the example.
  , txsModelFile    :: FilePath       -- ^ Path to the TorXakis model file.
  , txsCommandsFile :: FilePath       -- ^ Path to the file containing the commands
                                      --   that will be passed to the TorXakis server.
  , sutSourceFile   :: Maybe FilePath -- ^ Path to the SUT source code. This
                                      --   code will be compiled and run together with TorXakis. If
                                      --   this record is `Nothing` then the example is assumed to
                                      --   be autonomous (only TorXakis will be run)
  , expectedResult  :: ExampleResult  -- ^ Example's expected result.
  } deriving (Show)


-- | A set of examples.
data TxsExampleSet = TxsExampleSet
  { exampleSetName :: String       -- ^ Name of the example set.
  , txsExamples    :: [TxsExample] -- ^ Examples in the set.
  }

-- | Information about a compiled Java program.
data CompiledSut = JavaCompiledSut
  { mainClass    :: Text           -- ^ Name of the main Java class.
  , cpSearchPath :: Maybe FilePath -- ^ Class search path. If omitted no `-cp`
                                   --   option will be passed to the `java`
                                   --   command.
  }

-- | A processed example, ready to be run.
--
-- Currently the only processing that takes place is the compilation of the
-- SUT, if any.
data RunnableExample = ExampleWithSut TxsExample CompiledSut
                     | StandaloneExample TxsExample

data ExampleResult = Pass | Fail deriving (Show, Eq)

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

txsUIPassMsg :: Text
txsUIPassMsg = "TXS >>  PASS"

txsUIFailMsg :: Text
txsUIFailMsg = "TXS >>  FAIL"

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
    Nothing -> throwIO $ ProgramNotFound (pack (show cmd))
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
        "Java compilation command failed with code: " <> (pack . show) code
    ExitSuccess -> do
      mClass <- decodePath $ basename sourcePath
      let sPath = directory sourcePath
      return $ JavaCompiledSut mClass (Just sPath)

-- | Add the class path option if a class-path is given.
getCPOpts :: Maybe FilePath -> Test [Text]
getCPOpts Nothing         = return []
getCPOpts (Just filePath) = (("-cp":) . pure) <$> decodePath filePath

-- | Run TorXakis with the given example specification.
runTxsWithExample :: TxsExample -> IO (Either SqattError ())
runTxsWithExample ex = do
  eInputModelF <- runExceptT $ runTest $ decodePath (txsModelFile ex)
  case eInputModelF of
    Left decodeErr -> return $ Left decodeErr
    Right inputModelF -> do
      port <- repr <$> getFreePort
      res <-
        txsServerProc port `race` txsUIProc inputModelF port
      if Prelude.and (rights [res])
        then return $ Right ()
        else return $ Left tErr
  where
    txsUIProc imf port =
      Turtle.fold (inproc txsUICmd [port, imf] (input cmdsFile))
                  (Control.Foldl.any (isInfixOf searchStr . lineToText))
    cmdsFile = txsCommandsFile ex
    searchStr = expectedMsg . expectedResult $ ex
    tErr = TestExpectationError $
              format ("Did not get expected result "%s)
                     (repr . expectedResult $ ex)
    expectedMsg Fail = txsUIFailMsg
    expectedMsg Pass = txsUIPassMsg
    txsServerProc port = proc txsServerCmd [port] mempty

-- | Make a test out of a runnable example.
mkTest :: RunnableExample -> Test ()
mkTest (ExampleWithSut ex (JavaCompiledSut mClass cpSP)) = do
  cpOpts <- getCPOpts cpSP
  res <- liftIO $ sutProc cpOpts `race` runTxsWithExample ex
  case res of
    Left _              -> throwError SutAborted
    Right (Left txsErr) -> throwError txsErr
    Right (Right _)     -> return ()
  where
    sutProc cpOpts =
      proc javaCmd (cpOpts <> [mClass]) mempty
mkTest (StandaloneExample ex) = do
  res <- liftIO $ runTxsWithExample ex
  case res of
    Left txsErr -> throwError txsErr
    Right  _    -> return ()

-- | Get a free port number.
getFreePort :: IO Integer
getFreePort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
  port <- socketPort  sock
  close sock
  return (toInteger port)

-- | Execute a test.
execTest :: TxsExample -> IO (Either SqattError ())
execTest ex = runExceptT $ runTest $ do
  runnableExample <- getRunnableExample
  mkTest runnableExample
  where getRunnableExample =
          case sutSourceFile ex of
            Nothing ->
              return (StandaloneExample ex)
            Just sourcePath -> do
              cmpSut <- compileSut sourcePath
              return (ExampleWithSut ex cmpSut)

-- | Test a single example.
testExample :: TxsExample -> Spec
testExample ex = it (exampleName ex) $ do
  res <- execTest ex
  res `shouldBe` Right ()

-- | Test a list of examples.
testExamples :: [TxsExample] -> Spec
testExamples examples =
  beforeAll
    (cd $ ".." </> "..")
    (traverse_ testExample examples)

-- | Test an example set.
testExampleSet :: TxsExampleSet -> Spec
testExampleSet (TxsExampleSet esName exs) =
  describe esName (testExamples exs)

-- | Test a list of example sets.
testExampleSets :: [TxsExampleSet] -> Spec
testExampleSets = traverse_ testExampleSet
