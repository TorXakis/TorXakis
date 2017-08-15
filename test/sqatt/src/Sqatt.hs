-- | Integration test utilities.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Sqatt
  ( TxsExample(..)
  , checkSMTSolvers
  , checkCompilers
  , testExamples
  , ExampleResult (..)
  , javaCmd
  )
where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad.Except
import           Data.Foldable
import           Data.Monoid
import           Data.Text
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Network.Socket
import           Prelude                   hiding (FilePath)
import           System.Info
import           Test.Hspec
import           Turtle
import           Turtle.Prelude

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


data CompiledSut = JavaCompiledSut
  { mainClass    :: Text
  , cpSearchPath :: Maybe FilePath -- ^ Class search path. If omitted no `-cp`
                                   --   option will be passed to the `java`
                                   --   command.
  }

-- | An processed example, ready to be run.
--
-- Currently the only processing that takes place is the compilation of the
-- SUT, if any.
data RunnableExample = ExampleWithSut TxsExample CompiledSut
                     | StandaloneExample TxsExample

data ExampleResult = Pass | Fail deriving (Show)

-- | Check that all the supported SMT solvers are installed.
--
-- Throws an exception on failure.
checkSMTSolvers :: IO ()
checkSMTSolvers = do
  print "WARNING: The presence of SMT solvers was not checked."
  print "         First issue #47 needs to be resolved."
  print "See: https://github.com/TorXakis/TorXakis/issues/47"

addExeSuffix :: Text -> Text
addExeSuffix path = if os == "mingw32" then path <> ".exe" else path

javaCmd :: Text
javaCmd = addExeSuffix "java"

javacCmd :: Text
javacCmd = addExeSuffix "javac"

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

compileSut :: FilePath -> Test CompiledSut
compileSut sourcePath =
  case extension sourcePath of
    Just "java" ->
      compileJavaSut sourcePath
    _    -> do
      path <- decodePath sourcePath
      throwError $ UnsupportedLanguage $
        "Compiler not found for file " <> path

decodePath :: FilePath -> Test Text
decodePath filePath =
  case toText filePath of
    Right path ->
      return path
    Left apprPath ->
      throwError $ FilePathError $
        "Cannot decode " <> apprPath <> " properly"

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

newtype Test a = Test { runTest :: ExceptT SqattError IO a }
  deriving (Functor, Monad, Applicative, MonadError SqattError, MonadIO)




-- Some test to see how to call torxakis and the sut
runexample1 :: IO ()
runexample1 = do
  sutProc `race_` txsServerProc `race_` txsUIProc
  where sutProc = proc javaCmd ["-cp", example1cp, "StimulusResponse"] mempty
        txsUIProc = sh (inproc "txsui.exe" ["5000", inputModel] (input cmdsFile))
        txsServerProc = sh (inproc "txsserver.exe" ["5000"] mempty)
        inputModel = "..\\..\\examps\\stimulusresponse\\StimulusResponse.txs"
        example1cp = "..\\..\\examps\\stimulusresponse"
        cmdsFile = "..\\examps\\stimulusresponse\\StimulusResponse.txscmd"

mkTest :: RunnableExample -> Test ()
mkTest (ExampleWithSut ex cmpSut) = undefined
  -- Run TorXakis: txsserver.exe and txsui

  --
  -- inproc txui [portNr] input (txsCommandsFile ex)

  -- start /min torxakisServer.bat %PORTNR%
  -- txsui <portnumber> [input file(s)]

  -- Run the SUT

-- | Get a free port number.
getFreePort :: IO Integer
getFreePort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
  port <- socketPort  sock
  close sock
  return (toInteger port)

execTest :: TxsExample -> IO (Either SqattError ())
execTest ex = runExceptT $ runTest $ do
  runnableExample <- getRunnableExample ex
  mkTest runnableExample
  where getRunnableExample ex =
          case sutSourceFile ex of
            Nothing ->
              return (StandaloneExample ex)
            Just sourcePath -> do
              cmpSut <- compileSut sourcePath
              return (ExampleWithSut ex cmpSut)

data SqattError = CompileError Text
                | ProgramNotFound Text
                | UnsupportedLanguage Text
                | FilePathError Text
  deriving (Show, Eq)

instance Exception SqattError

-- TODO: do we need to return a Spec at all? Maybe to use the `it` function...
testExample :: TxsExample -> Spec
testExample ex = it (exampleName ex) $ do
  res <- execTest ex
  res `shouldBe` Right ()

testExamples :: [TxsExample] -> Spec
testExamples examples  = do
  beforeAll (cd $ ".." </> "..")
            (traverse_ testExample examples)
