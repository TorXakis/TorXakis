-- | Integration test utilities.
{-# LANGUAGE OverloadedStrings #-}
module Sqatt
  ( TxsExample(..)
  , checkSMTSolvers
  , checkCompilers
  , testExamples
  , ExampleResult (..)
  , javaCmd
  )
where

import           Control.Exception
import           Data.Foldable
import           Data.Text
import           Filesystem.Path
import           Prelude           hiding (FilePath)
import           System.Info
import           Test.Hspec
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
  }


data CompiledSut = JavaCompiledSut
  { mainClass    :: String
  , cpSearchPath :: Maybe FilePath -- ^ Class search path. If omitted no `-cp`
                                   -- option will be passed to the `java`
                                   -- command.
  }

-- | An processed example, ready to be run.
--
-- Currently the only processing that takes place is the compilation of the
-- SUT, if any.
data CompiledExample = CompiledExample TxsExample (Maybe CompiledSut)

data ExampleResult = Pass | Fail

-- | Check that all the supported SMT solvers are installed.
--
-- Throws an exception on failure.
checkSMTSolvers :: IO ()
checkSMTSolvers = do
  print "WARNING: The presence of SMT solvers was not checked."
  print "         First issue #47 needs to be resolved."
  print "See: https://github.com/TorXakis/TorXakis/issues/47"

exeSuffix :: Text
exeSuffix = if os == "mingw32" then "exe" else mempty

javaCmd :: FilePath
javaCmd = "java" <.> exeSuffix

javacCmd :: FilePath
javacCmd = "javac" <.> exeSuffix

-- | Check that the given command exists in the search path of the host system.
checkCommand :: FilePath -> IO ()
checkCommand cmd = do
  path <- which cmd
  case path of
    Nothing -> throwIO $ ProgramNotFound (show cmd)
    _       -> return ()

-- | Check that all the compilers are installed.
--
-- Throws an exception on failure.
checkCompilers :: IO ()
checkCompilers = traverse_ checkCommand [javaCmd, javacCmd]

compileExample :: TxsExample -> IO (Either String CompiledExample)
compileExample = undefined

runExample :: CompiledExample -> IO ()
runExample = undefined

data SqattException = CompileError String | ProgramNotFound String
  deriving Show

instance Exception SqattException

-- TODO: do we need to return a Spec at all? Maybe to use the `it` function...
testExample :: TxsExample -> Spec
testExample ex = it (exampleName ex) $ do
  res <- compileExample ex
  case res of
    Left err  -> throwIO $ CompileError err
    Right cEx -> runExample cEx

testExamples :: [TxsExample] -> Spec
testExamples = traverse_ testExample
