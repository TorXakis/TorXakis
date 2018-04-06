{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Examples of usage of 'TorXakis.Lib'.
--
-- This file is meant to server as an example of usage of the 'TorXakis.Lib'.
-- These examples can be used as reference for developers of tools on top this
-- library.

module TorXakis.Lib.Examples where

import           Control.Concurrent.Async     (async, cancel)
import           Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import           Control.Concurrent.STM.TVar  (readTVarIO)
import           Control.Exception            (SomeException, catch)
import           Control.Monad                (void)
import           Control.Monad.State          (StateT, evalStateT, lift)
import           Control.Monad.STM            (atomically)
import           Data.Conduit                 (runConduit, (.|))
import           Data.Conduit.Combinators     (mapM_, sinkList, take)
import           Data.Conduit.TQueue          (sourceTQueue)
import           Data.Foldable                (traverse_)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import           Lens.Micro                   ((&), (.~), (^.))
import           Prelude                      hiding (mapM_, take)

import           ChanId                       (ChanId (ChanId))
import           ConstDefs                    (Const (Cstr, Cstring), cstrId)
import           CstrId                       (CstrId (CstrId), name)
import           EnvBTree                     (EnvB (EnvB), msgs, smts, stateid)
import           TxsDefs                      (sortDefs)

import qualified EnvBTree                     as E
import           EnvData                      (Msg)
import           Eval                         (eval)
import           FuncTable                    (FuncTable, Signature (Signature),
                                               signHandler)
import           Id                           (Id (Id))
import           Name                         (Name)
import           SortId                       (SortId (SortId))
import           SortOf                       (sortOf)
import           TorXakis.Lens.ModelDef
import           TorXakis.Lens.Sigs           (funcTable)
import           TorXakis.Lens.TxsDefs
import           TorXakis.Lib
import           TorXakis.Lib.Internal
import           TorXakis.Lib.Session
import           TxsDDefs                     (Action (Act, ActQui), Verdict)
import           ValExpr                      (ValExpr, cstrConst)
import           VarId                        (VarId)

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
testTorXakisWithInfo :: IO (Either SomeException Verdict)
testTorXakisWithInfo = do
    -- TODO: We should start the web server at this point.
    s <- newSession
    cs <- readFile "../../examps/TorXakisWithEcho/TorXakisWithEchoInfoOnly.txs"
--    cs <- readFile "../../examps/TorXakisWithEcho/TorXakisWithEcho.txs"
    _ <- load s cs
    st <- readTVarIO (s ^. sessionState)
    let
        Just mDef = st ^. tdefs . ix ("Model" :: Name)
        mSendToW :: ToWorldMapping -- [Const] -> IO (Maybe Action)
        mSendToW = ToWorldMapping $ \xs ->
            case xs of
                [Cstr {cstrId = CstrId { name = "CmdInfo"}} ] -> do
                    let [setChId] = mDef ^. modelOutChans
                        [outChId] = Set.toList setChId
                        ft = st ^. sigs . funcTable
                        params :: [ValExpr VarId]
                        params = [ cstrConst (Cstring "0.0.1"), cstrConst (Cstring "Today")]
                        [sId] = [ SortId n i
                                | (SortId n i, _) <- Map.toList $ sortDefs (st ^. tdefs)
                                ,  n == "Response" ]
                    Right res <- apply st ft "ResponseInfo" params sId
                    return $ Just $ Act [(outChId, [res])]
                _   -> error $ "Didn't expect this data on channel " ++ show chanId
                            ++ " (got " ++ show xs ++ ")"
        chanId :: ChanId
        chanId = let [setChId] = mDef ^. modelInChans
                     [res] = Set.toList setChId
                 in res
        s' :: Session
        s' = s & wConnDef . toWorldMappings .~ Map.singleton chanId mSendToW
    _ <- tester s' "Model"
    _ <- test s' (NumberOfSteps 10)
    a <- async (printer s')
    v <- waitForVerdict s'
    cancel a
    return v

-- | TODO: look whether this isn't defined somewhere else. If not make it safer
-- by not throwing an error if the lookup returns nothing.
apply :: SessionSt -> FuncTable VarId -> Text -> [ValExpr VarId] -> SortId -> IO (Either String Const)
apply st ft fn vs sId = do
    let Just f = Map.lookup sig (signHandler fn ft)
        sig = Signature (sortOf <$> vs) sId
        envB = EnvB
            { smts = Map.empty
            , E.tdefs = st ^. tdefs
            , E.sigs = st ^. sigs
            , stateid = -1
            , E.unid = 10000
            , E.params = Map.empty
            , msgs = []
            }
    evalStateT (eval (f vs)) envB

-- import           Lens.Micro
-- import           Control.Concurrent.STM.TVar (readTVarIO)
-- import FuncTable
-- import ValExpr
-- import ConstDefs
-- import TorXakis.Lens.Sigs
-- import qualified Data.Map as Map

-- s <- newSession
-- cs <- readFile "examps/TorXakisWithEcho/TorXakisWithEchoInfoOnly.txs"
-- load s cs
-- st <- readTVarIO (s ^. sessionState)
-- let ft = st ^. sigs . funcTable
-- let params = [ cstrConst (Cstring "Foo"), cstrConst (Cstring "Bar")]
-- let [sId] = [ SortId n id | (SortId n id, _) <- Map.toList $ sortDefs (st ^. tdefs) ,  n == "Response" ]
-- res <- apply st ft params sId

testPutToWReadsWorld :: IO Bool
testPutToWReadsWorld = do
    s <- newSession
    let fWCh = s ^. fromWorldChan
        txsChanId = ChanId "DummyChan" (Id 42) [SortId "DummySort" (Id 43)]
        actP = Act $ Set.fromList [(txsChanId, [Cstring "Action NOT to be put"])]
        actG = Act $ Set.fromList [(txsChanId, [Cstring "Action to be gotten"])]
        fakeSendToW :: ToWorldMapping
        fakeSendToW = error "This function should not be called in testPutToWReadsWorld, since there's an action waiting in fromWorldChan."
        toWMMs = Map.singleton txsChanId fakeSendToW
    atomically $ writeTChan fWCh actG
    act <- runIOC s $ putToW fWCh toWMMs actP
    atomically $ writeTChan fWCh actG
    act' <- runIOC s $ putToW fWCh Map.empty ActQui
    return $ act == actG && act' == actG

