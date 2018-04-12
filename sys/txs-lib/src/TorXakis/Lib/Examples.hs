{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | Examples of usage of 'TorXakis.Lib'.
--
-- This file is meant to server as an example of usage of the 'TorXakis.Lib'.
-- These examples can be used as reference for developers of tools on top this
-- library.

module TorXakis.Lib.Examples where

import           Control.Concurrent           (ThreadId, forkIO)
import           Control.Concurrent.Async     (async, cancel)
import           Control.Concurrent.STM.TChan (TChan, writeTChan)
import           Control.Concurrent.STM.TVar  (readTVarIO)
import           Control.Exception            (SomeException)
import           Control.Monad                (void, when)
import           Control.Monad.State          (evalStateT)
import           Control.Monad.STM            (atomically)
-- import           Data.Aeson                   (decode)
import           Data.Aeson.Lens              (key)
import           Data.Aeson.Types             (Value (String))
import qualified Data.ByteString.Char8        as BS
-- import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.Conduit                 (runConduit, (.|))
import           Data.Conduit.Combinators     (mapM_, sinkList, take)
import           Data.Conduit.TQueue          (sourceTQueue)
import           Data.Foldable                (traverse_)
import qualified Data.Map                     as Map
import           Data.Monoid                  ((<>))
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lens.Micro                   ((&), (.~), (^.), (^?))
import           Prelude                      hiding (mapM_, take)
import           System.Process               (StdStream (NoStream), proc,
                                               std_out, withCreateProcess)

import           ChanId                       (ChanId (ChanId))
import           ConstDefs                    (Const (Cstr, Cstring), args,
                                               cString, cstrId)
import           CstrId                       (CstrId (CstrId), name)
import           EnvBTree                     (EnvB (EnvB), msgs, smts, stateid)
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
import           TxsDefs                      (ModelDef, sortDefs)
import           ValExpr                      (ValExpr, cstrConst)
import           VarId                        (VarId)

import           Network.Wreq                 (foldGet, get, partFile, partText,
                                               post, responseBody,
                                               responseStatus, statusCode)

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

-- | Test info
--
-- TODO: for now I'm putting this test here. We should find the right place for
-- this test. Once a new command line interface for TorXakis which uses
-- 'txs-lib' is ready we can proceed with removing this test.
testTorXakisWithInfo :: IO (Either SomeException Verdict)
testTorXakisWithInfo = withCreateProcess (proc "txs-webserver-exe" []) {std_out = NoStream} $ \_stdin _stdout _stderr _ph -> do
    s <- newSession
    cs <- readFile "../../examps/TorXakisWithEcho/TorXakisWithEchoInfoOnly.txs"
    _ <- load s cs
    st <- readTVarIO (s ^. sessionState)
    let Just mDef = st ^. tdefs . ix ("Model" :: Name)
        outChId = getOutChanId mDef
        inChId  = getInChanId  mDef
        mSendToW :: ToWorldMapping -- [Const] -> IO (Maybe Action)
        mSendToW = ToWorldMapping $ \xs ->
            case xs of
                [Cstr {cstrId = CstrId { name = "CmdInfo"}} ] ->
                    -- This is where we send the command through HTTP
                    Just <$> actInfo st outChId
                _   -> error $ "Didn't expect this data on channel " ++ show inChId
                            ++ " (got " ++ show xs ++ ")"
        s' :: Session
        s' = s & wConnDef . toWorldMappings .~ Map.singleton inChId mSendToW
    _ <- tester s' "Model"
    _ <- test s' (NumberOfSteps 10)
    a <- async (printer s')
    v <- waitForVerdict s'
    cancel a
    return v

testTorXakisWithEcho :: IO (Either SomeException Verdict)
testTorXakisWithEcho = withCreateProcess (proc "txs-webserver-exe" []) {std_out = NoStream} $ \_stdin _stdout _stderr _ph -> do
    s <- newSession
    cs <- readFile "../../examps/TorXakisWithEcho/TorXakisWithEcho.txs"
    _ <- load s cs
    st <- readTVarIO (s ^. sessionState)
    let Just mDef = st ^. tdefs . ix ("Model" :: Name)
        outChId = getOutChanId mDef
        inChId  = getInChanId  mDef
        mSendToW :: ToWorldMapping -- [Const] -> IO (Maybe Action)
        mSendToW = ToWorldMapping $ \xs ->
            case xs of
                [Cstr { cstrId = CstrId { name = "CmdInfo" }} ] -> Just <$> actInfo st outChId
                [Cstr { cstrId = CstrId { name = "CmdLoad" }
                      , args = [Cstring { cString = fileToLoad }]
                      }] -> Just <$> actLoad st outChId fileToLoad
                _   -> error $ "Didn't expect this data on channel " ++ show inChId
                            ++ " (got " ++ show xs ++ ")"
        mInitWorld :: TChan Action -> IO [ThreadId]
        mInitWorld fWCh = do
            tid <- forkIO $ do
                let
                    writeToChan :: TChan Action -> BS.ByteString -> IO (TChan Action)
                    writeToChan ch bs = do -- TODO: convert json to Msg, extract Act and write to chan
                        putStrLn $ "SSE says: " ++ (head . lines . BS.unpack) bs
                        return ch
                putStrLn "mInitWorld is called"
                actInit
                _ <- foldGet writeToChan fWCh "http://localhost:8080/session/sse/1/messages"
                return ()
            return [tid]
        s' :: Session
        s' = s & wConnDef . toWorldMappings .~ Map.singleton inChId mSendToW
               & wConnDef . initWorld .~ mInitWorld
    _ <- tester s' "Model"
    _ <- test s' (NumberOfSteps 20)
    a <- async (printer s')
    v <- waitForVerdict s'
    cancel a
    return v

actInfo :: SessionSt -> ChanId -> IO Action
actInfo st outChId = do
    -- In this case it's just a GET request
    -- TODO: This address should be extracted from Model CNECTDEF
    resp <- get "http://localhost:8080/info"
    let status = resp ^. responseStatus . statusCode
    if status /= 200
        then error $ "/info returned unxpected status: " ++ show status
        else do -- TODO: This ResponseInfo should be in responseBody as JSON
            let params = infoParams resp
                [sId] = [ SortId n i
                    | (SortId n i, _) <- Map.toList $ sortDefs (st ^. tdefs)
                    ,  n == "Response" ]
                ft = st ^. sigs . funcTable
            res <- apply st ft "ResponseInfo" params sId
            case res of
                Right cnst -> return $ Act $ Set.fromList [(outChId, [cnst])]
                Left  err  -> error $ "Can't create Action because: " ++ err
              where
                infoParams r =
                    let Just (String version'  ) = r ^? responseBody . key "version"
                        Just (String buildTime') = r ^? responseBody . key "buildTime"
                    in  [cstrConst (Cstring version')
                        , cstrConst (Cstring buildTime')]

actLoad ::  SessionSt -> ChanId -> Text -> IO Action
actLoad st outChId path = do
    putStrLn $ "actLoad POST request to upload: " ++ T.unpack path
    -- TODO: This address should be extracted from Model CNECTDEF
    resp <- post "http://localhost:8080/session/1/model" [partFile "txs" (T.unpack $ "..\\..\\" <> path)]
    case resp ^. responseStatus . statusCode of
        201 -> do putStrLn $ "actLoad /session/1/model received: " ++ show resp
                  let [sId] = [ SortId n i
                                | (SortId n i, _) <- Map.toList $ sortDefs (st ^. tdefs)
                              ,  n == "Response" ]
                      ft = st ^. sigs . funcTable
                  res <- apply st ft "ResponseSuccess" [] sId
                  case res of
                        Right cnst -> return $ Act $ Set.fromList [(outChId, [cnst])]
                        Left  err  -> error $ "Can't create Action because: " ++ err
        s   -> error $ "/session/1/model returned unxpected status: " ++ show s

actInit :: IO ()
actInit = do
    putStrLn "actInit POST request to create new session"
    resp <- post "http://localhost:8080/session/new" [partText "" ""]
    let status = resp ^. responseStatus . statusCode
    when (status /= 201) $
        error $ "/session/new returned unxpected status: " ++ show status
    putStrLn $ "actInit /session/new received: " ++ show resp

apply :: SessionSt -> FuncTable VarId -> Text -> [ValExpr VarId] -> SortId -> IO (Either String Const)
apply st ft fn vs sId = do
    let sig = Signature (sortOf <$> vs) sId
    case Map.lookup sig (signHandler fn ft) of
        Nothing -> return $ Left $ "No handler found for sig:" ++ show sig
        Just f  ->
            let envB = EnvB
                        { smts = Map.empty
                        , E.tdefs = st ^. tdefs
                        , E.sigs = st ^. sigs
                        , stateid = -1
                        , E.unid = 10000
                        , E.params = Map.empty
                        , msgs = []
                        }
            in evalStateT (eval (f vs)) envB -- TODO: Should not use eval, just parse a Response with new ADTDefs.

getOutChanId :: ModelDef -> ChanId
getOutChanId mDef =
    let [setChId] = mDef ^. modelOutChans
        [outChId] = Set.toList setChId
    in  outChId

getInChanId :: ModelDef -> ChanId
getInChanId mDef =
    let [setChId] = mDef ^. modelInChans
        [inChId] = Set.toList setChId
    in inChId
