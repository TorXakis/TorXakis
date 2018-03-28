{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | Examples of usage of 'TorXakis.Lib'.
--
-- This file is meant to server as an example of usage of the 'TorXakis.Lib'.
-- These examples can be used as reference for developers of tools on top this
-- library.

module TorXakis.Lib.Examples where

import           Prelude                  hiding (mapM_, take)

import           Control.Concurrent.Async (async, cancel)
import           Control.Monad            (void)
import           Data.Conduit             (runConduit, (.|))
import           Data.Conduit.Combinators (mapM_, sinkList, take)
import           Data.Conduit.TQueue      (sourceTQueue)
import           Data.Foldable            (traverse_)
import           Lens.Micro               ((^.))

import           EnvData                  (Msg)

import           TorXakis.Lib
import           TorXakis.Lib.Session

-- | Get the next N messages in the session.
--
-- If no messages are available this call will block waiting for new messages.
-- Normally one won't use such a function, but it is useful when playing with
-- the core 'TorXakis' function in an GHCi session.
--
getNextNMsgs :: Session -> Int -> IO [Msg]
getNextNMsgs s n =
    runConduit $ sourceTQueue (s ^. sessionMsgs) .| take n .| sinkList

-- | Get and print the next messages.
printNextNMsgs :: Session -> Int -> IO ()
printNextNMsgs s n = getNextNMsgs s n >>= traverse_ print

-- | This example shows how to load a 'TorXakis' model, and run a couple of
-- steps with the stepper, leveraging on @Conduit@s for printing messages as
-- they become available.
testEchoReactive :: IO ()
testEchoReactive = do
    cs <- readFile "test/data/Echo.txs"
    s <- newSession
    -- Spawn off the printer process:
    a <- async (printer s)
    -- Load the model:
    r <- load s cs
    putStrLn $ "Result of `load`: " ++ show r
    -- Start the stepper:
    r' <- stepper s "Model"
    putStrLn $ "Result of `stepper`: " ++ show r'
    -- Run a couple of steps:
    r'' <- step s (NumberOfSteps 10)
    -- Note that the step function is asynchronous: it will return immediately:
    putStrLn $ "Result of `step`: " ++ show r''
    -- Run a couple more of steps:
    void $ step s (NumberOfSteps 10)
    -- Wait for the first verdict:
    waitForVerdict s >>= print
    -- Wait for the second verdict:
    waitForVerdict s >>= print
    -- Cancel the printer (we aren't interested in any more messages, as a
    -- verdict has been reached):
    cancel a
  where
    printer :: Session -> IO ()
    printer s = runConduit $
        sourceTQueue (s ^. sessionMsgs) .|  mapM_ print

-- | This example shows what happens when you load an invalid file.
testWrongFile :: IO Response
testWrongFile = do
    cs <- readFile "test/data/wrong.txt"
    s <- newSession
    -- Load the model:
    r <- load s cs
    putStrLn $ "Result of `load wrong.txt`: " ++ show r
    return r

testInfo :: IO ()
testInfo = case info of
    Info _v _b -> return ()

-- | Test info
--
-- TODO: for now I'm putting this test here. We should find the right place for
-- this test. Once a new command line interface for TorXakis which uses
-- 'txs-lib' is ready we can proceed with removing this test.
testTorXakisWithInfo :: IO Response
testTorXakisWithInfo = do
    -- TODO: We should start the web server at this point.
    s <- newSession
    cs <- readFile "../../examps/TorXakisWithEcho/TorXakisWithEchoInfoOnly.txs"
    r <- load s cs
    _ <- tester s "Model"
    -- test s 100
    return r
