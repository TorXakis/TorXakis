{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}

-- |
module TorXakis.Lib where

import           Control.Arrow                 ((|||))
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.MVar       (newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM.TChan  (newTChanIO)
import           Control.Concurrent.STM.TQueue (TQueue, isEmptyTQueue,
                                                newTQueueIO, readTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (modifyTVar', newTVarIO,
                                                readTVarIO)
import           Control.DeepSeq               (force)
import           Control.Exception             (ErrorCall, SomeException, catch,
                                                evaluate, try)
import           Control.Monad                 (unless, void)
import           Control.Monad.State           (lift, runStateT)
import           Control.Monad.STM             (atomically, retry)
import           Control.Monad.Trans.Except    (ExceptT, runExceptT, throwE)
import           Data.Aeson                    (ToJSON)
import           Data.Foldable                 (traverse_)
import           Data.Map.Strict               as Map
import           Data.Semigroup                ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import           Lens.Micro                    ((&), (.~), (^.))

import qualified BuildInfo
import qualified VersionInfo

import           EnvCore                       (IOC)
import           EnvData                       (Msg (TXS_CORE_SYSTEM_ERROR))
import           Name                          (Name)
import           TorXakis.Lens.TxsDefs         (ix)
import           TxsAlex                       (txsLexer)
import           TxsCore                       (txsInitCore, txsSetCore,
                                                txsTermitCore)
import           TxsStep                       (txsSetStep, txsShutStep,
                                                txsStartStep, txsStepRun,
                                                txsStopStep)
-- import           TxsCore                       (txsInit, txsSetStep, txsSetTest,
--                                                 txsStepN, txsStop, txsTestN)
import           TxsDDefs                      (Verdict)
import           TxsDefs                       (ModelDef)
import           TxsHappy                      (txsParser)

import           TorXakis.Lib.Session

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

-- TODO: do we need a String here?
data Response = Success | Error { msg :: String } deriving (Eq, Show)

isError :: Response -> Bool
isError (Error _) = True
isError _         = False

data TorXakisInfo = Info { version :: String, buildTime :: String }
    deriving (Generic)
instance ToJSON TorXakisInfo

info :: TorXakisInfo
info = Info VersionInfo.version BuildInfo.buildTime

-- | Create a new session.
newSession :: IO Session
newSession = Session <$> newTVarIO emptySessionState
                     <*> newTQueueIO
                     <*> newMVar ()
                     <*> newTQueueIO
                     <*> newTChanIO
                     <*> return (WorldConnDef Map.empty (\_ -> return []))
                     <*> return []

-- | Stop a session.
killSession :: Session -> IO Response
killSession _s  =  do
    return $ Error "Kill Session: Not implemented (yet)"

-- | Load a TorXakis file, compile it, and return the response.
--
-- The absolute path to the file is used to associate the parsed structure
-- (using the file-contents) with the file name, so that by calling `unload` we
-- know what to unload.
--
-- In the future we might want to make loading of 'TorXakis' models
-- incremental, to give better error messages, and support more modularity.
load :: Session -> FileContents -> IO Response
load s xs = do
    r <- try $ do -- Since the 'TorXakis' parser currently just calls 'error'
                  -- we have to catch a generic 'ErrorCall' exception.
        (_, ts, is) <- evaluate . force . txsParser . txsLexer $ xs
        atomically $ modifyTVar' (_sessionState s) ((tdefs .~ ts) . (sigs .~ is))
        return ()
    case r of
        Left err -> return $ Error $ show (err :: ErrorCall)
        Right _  -> do
            -- Initialize the TorXakis core with the definitions we just loaded.
            st <- readTVarIO (s ^. sessionState)
            runIOC s $ do
                resp <- txsInitCore (st ^. tdefs) (st ^. sigs) (msgHandler (_sessionMsgs s))
                case resp of
                    Right _ -> return Success
                    Left  e -> return $ Error e


-- | Start the stepper with the given model.
stepper :: Session
        -> Name        -- ^ Model name
        -> IO Response
stepper s mn = runResponse $ do
    mDef <- lookupModel s mn
    lift $ runIOC s $ do txsSetStep mDef
                         txsStartStep

lookupModel :: Session -> Name -> ExceptT Text IO ModelDef
lookupModel s mn = do
    st <- lift $ readTVarIO (s ^. sessionState)
    maybe
        (throwE $ "No model named " <> mn)
        return (st ^. tdefs . ix mn)

-- | Leave the stepper.
shutStepper :: Session -> IO Response
shutStepper s  =  do
    runIOC s $ do
        txsStopStep
        txsShutStep
        return Success

msgHandler :: TQueue Msg -> [Msg] -> IOC ()
msgHandler q = lift . atomically . traverse_ (writeTQueue q)

-- | How a step is described
data StepType =  NumberOfSteps Int
    --         | ActionTxt String -- todo: Make this a TorXakis Action
    --           | GoTo StateNumber
    --           | Reset -- ^ Go to the initial state.
    --           | Rewind Steps
-- data StateNumber
-- data ActionName
-- TODO: discuss with Jan: do we need a `Tree` step here?

-- | Step for n-steps or actions
step :: Session -> StepType -> IO Response
step s (NumberOfSteps n) = do
    void $ forkIO $ do
        verd <- try $ runIOC s $ txsStepRun n
        atomically $ writeTQueue (s ^. verdicts) verd
    return Success

-- | Wait for a verdict to be reached.
waitForVerdict :: Session -> IO (Either SomeException Verdict)
waitForVerdict s = atomically $ readTQueue (s ^. verdicts)

-- | Wait for the message queue to be consumed.
waitForMessageQueue :: Session -> IO ()
waitForMessageQueue s = atomically $ do
    b <- isEmptyTQueue (s ^. sessionMsgs)
    unless b retry

eitherToResponse :: Either Text () -> Response
eitherToResponse = Error . T.unpack ||| const Success

runResponse :: ExceptT Text IO () -> IO Response
runResponse act = eitherToResponse <$> runExceptT act

-- | Start the tester
tester :: Session
       -> Name
       -> IO Response
tester s mn = runResponse $ do
    mDef <- lookupModel s mn
    lift $ do
        let fWCh = s ^. fromWorldChan
        st <- readTVarIO (s ^. sessionState)
        tids <- (s ^. wConnDef . initWorld) fWCh
        let Just (deltaString,_) = Map.lookup "param_Sut_deltaTime" (st ^. prms)
            deltaTime = undefined -- read deltaString
            s' = s & worldListeners .~ tids
        runIOC s' $ undefined
            -- TODO: `txsSetTest` this was commented out by Jan.
            -- txsSetTest (putToW fWCh (s' ^. wConnDef . toWorldMappings))
            --            (getFromW deltaTime fWCh)
            --            mDef Nothing Nothing

-- | Test for n-steps
test :: Session -> StepType -> IO Response
test s (NumberOfSteps n) = do
    void $ forkIO $ do undefined
        -- TODO: `txsTestN` this was commented out by Jan.
        -- verdict <- try $ runIOC s $ txsTestN n
        -- atomically $ writeTQueue (s ^. verdicts) verdict
    return Success

-- | Start the stepper with the given model.
stop :: Session -> IO Response
stop s = undefined
    -- TODO: `txsStop` was commented out by Jan.
    -- runResponse $ lift $ runIOC s txsStop

-- | Run an IOC action, using the initial state provided at the session, and
-- modifying the end-state accordingly.
--
-- Two `runIOC` action won't be run in parallel. If an IOC action is pending,
-- then a subsequent call to `runIOC` will block till the operation is
-- finished.
--
runIOC :: Session -> IOC a -> IO a
runIOC s act = runIOC' `catch` reportError
    where
      runIOC' = do
          -- The GHC implementation of MVar's guarantees fairness in the access to
          -- the critical sections delimited by `takeMVar` and `putMVar`.
          takeMVar (s ^. pendingIOC)
          st <- readTVarIO (s ^. sessionState)
          (r, st') <- runStateT act (st ^. envCore)
          atomically $ modifyTVar' (s ^. sessionState) (envCore .~ st')
          putMVar (s ^. pendingIOC) ()
          return r
      reportError :: SomeException -> IO a
      reportError err = do
          -- There's no pending IOC anymore, we release the lock.
          putMVar (s ^. pendingIOC) ()
          atomically $ writeTQueue (s ^. sessionMsgs) (TXS_CORE_SYSTEM_ERROR (show err))
          error (show err)
