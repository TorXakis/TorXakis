-- |
module TorXakis.Lib where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.MVar       (newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM.TQueue (TQueue, isEmptyTQueue,
                                                newTQueueIO, readTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (modifyTVar', newTVarIO,
                                                readTVarIO)
import           Control.DeepSeq               (force)
import           Control.Exception             (ErrorCall, evaluate, try)
import           Control.Monad                 (void)
import           Control.Monad.State           (lift, runStateT)
import           Control.Monad.STM             (atomically)
import           Data.Foldable                 (traverse_)
import           Lens.Micro                    ((.~), (^.))

import           EnvCore                       (IOC)
import           EnvData                       (Msg)
import           Name                          (Name)
import           TorXakis.Lens.TxsDefs         (ix)
import           TxsAlex                       (txsLexer)
import           TxsCore                       (txsInit, txsSetStep, txsStepN)
import           TxsDDefs                      (Verdict)
import           TxsHappy                      (txsParser)

import           TorXakis.Session

data Response = Success | Error { msg :: String } deriving (Show)

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

-- | Create a new session.
newSession :: IO Session
newSession = Session <$> newTVarIO emptySessionState
                     <*> newTQueueIO
                     <*> newMVar ()
                     <*> newTQueueIO

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
        Right _  -> return Success


-- | Start the stepper with the given model.
stepper :: Session
        -> Name        -- ^ Model name
        -> IO Response
stepper s mn =  do
    st <- readTVarIO (s ^. sessionState)
    runIOC s $ do
        txsInit (st ^. tdefs) (st ^. sigs) (msgHandler (_sessionMsgs s))
        -- Lookup the model definition that matches the name.
        let mMDef = st ^. tdefs . ix mn
        case mMDef of
            Nothing ->
                return $ Error $ "No model named " ++ show mn
            Just mDef -> do
                txsSetStep mDef
                return Success

msgHandler :: TQueue Msg -> [Msg] -> IOC ()
msgHandler q = lift . atomically . traverse_ (writeTQueue q)

-- | How a step is described
newtype StepType = NumberOfSteps Int
    -- Action ActionName
    --           |
    --           | GoTo StateNumber
    --           | Reset -- ^ Go to the initial state.
    --           | Rewind Steps
-- data StateNumber
-- data ActionName
-- TODO: discuss with Jan: do we need a `Tree` step here?

-- | Step for n-steps
step :: Session -> StepType -> IO Response
step s (NumberOfSteps n) = do
    void $ forkIO $ runIOC s $ do
        verdict <- txsStepN n
        lift $ atomically $ writeTQueue (s ^. verdicts) verdict
    return Success

-- | Wait for a verdict to be reached.
waitForVerdict :: Session -> IO Verdict
waitForVerdict s = atomically $ readTQueue (s ^. verdicts)

-- | Wait for the message queue to be consumed.
waitForMessageQueue :: Session -> IO ()
waitForMessageQueue s = void $ atomically $ isEmptyTQueue (s ^. sessionMsgs)

-- | Run an IOC action, using the initial state provided at the session, and
-- modifying the end-state accordingly.
--
-- Two `runIOC` action won't be run in parallel. If an IOC action is pending,
-- then a subsequent call to `runIOC` will block till the operation is
-- finished.
--
runIOC :: Session -> IOC a -> IO a
runIOC s act = do
    -- The GHC implementation of MVar's guarantees fairness in the access to
    -- the critical sections delimited by `takeMVar` and `putMVar`.
    takeMVar (s ^. pendingIOC)
    st <- readTVarIO (s ^. sessionState)
    (r, st') <- runStateT act (st ^. envCore)
    atomically $ modifyTVar' (s ^. sessionState) (envCore .~ st')
    putMVar (s ^. pendingIOC) ()
    return r
