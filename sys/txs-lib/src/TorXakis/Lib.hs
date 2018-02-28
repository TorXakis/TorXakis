-- |
module TorXakis.Lib where

import           Prelude hiding (take)

import           Control.Concurrent.STM.TVar   (newTVarIO, readTVarIO, modifyTVar')
import           Control.Monad.STM             (atomically)
import           Control.Exception             (try, ErrorCall)
import           Control.Monad.State           (runStateT, lift)
import           Lens.Micro                    ((.~),(^.))
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue)
import           Data.Foldable                 (traverse_)
import           Data.Conduit                  (runConduit,(.|))
import           Data.Conduit.Combinators      (take, sinkList)
import           Data.Conduit.TQueue           (sourceTQueue)
import           Control.Concurrent            (forkIO)
import           Control.Monad                 (void)

import           TxsAlex  (txsLexer)
import           TxsHappy (txsParser)
import           EnvCore  (IOC)
import           TxsCore  (txsInit, txsSetStep, txsStepN)
import           EnvData  (Msg (TXS_CORE_RESPONSE))
import           TorXakis.Lens.TxsDefs (ix)
import           Name     (Name)

import           TorXakis.Session

data Response = Success | Error { msg :: String } deriving (Show)

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

-- | Create a new session.
newSession :: IO Session
newSession = Session <$> newTVarIO emptySessionState <*> newTQueueIO

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
        let (_, ts, is) = txsParser . txsLexer $ xs
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

-- | Get the next message in the session.
getNextMsg :: Session -> IO [Msg]
getNextMsg s =
    runConduit $ sourceTQueue (s ^. sessionMsgs) .| take 1 .| sinkList

-- | Get the next N messages in the session.
--
-- If no messages are available this call will block waiting for new messages.
--
-- TODO: I don't know if this will be used in practice, it is more for
-- debugging purposes.
getNextNMsgs :: Session -> Int -> IO [Msg]
getNextNMsgs s n =
    runConduit $ sourceTQueue (s ^. sessionMsgs) .| take n .| sinkList

-- | TODO: this is also used for debugging purposes. Put this into an
-- `Examples` or `Test` folder.
--
-- In such an example folder we might want to include a conduit version of it.
printNextNMsgs :: Session -> Int -> IO ()
printNextNMsgs s n = getNextNMsgs s n >>= traverse_ print

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

-- | step for n-steps
step :: Session -> StepType -> IO Response
step s (NumberOfSteps n) = do
    void $ forkIO $ runIOC s $ do
        verdict <- txsStepN n
        msgHandler (s ^. sessionMsgs) [TXS_CORE_RESPONSE (show verdict)]
    return Success

-- | Run an IOC action, using the initial state provided at the session, and
-- modifying the end-state accordingly.
-- 
-- TODO: Note that this function has a race condition since the session state
-- might have been altered by the time the IOC action returns the new state.
-- This seems to be a problem inherent to TorXakis, so to workaround this we
-- might have to introduce a transactional variable which signals the fact that
-- a state update operation is pending on the session.
runIOC :: Session -> IOC a -> IO a
runIOC s act = do
    st <- readTVarIO (s ^. sessionState)
    (r, st') <- runStateT act (st ^. envCore)
    atomically $ modifyTVar' (s ^. sessionState) (envCore .~ st')
    return r
