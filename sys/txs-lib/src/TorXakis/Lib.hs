-- |
module TorXakis.Lib where

import           Control.Concurrent.STM.TVar   (newTVarIO, readTVarIO, modifyTVar')
import           Control.DeepSeq               (force)
import           Control.Monad.STM             (atomically)
import           Control.Exception             (try, ErrorCall, evaluate)
import           Control.Monad.State           (runStateT, lift)
import           Lens.Micro                    ((.~),(^.))
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import           Data.Foldable                 (traverse_)
import           Control.Concurrent            (forkIO)
import           Control.Monad                 (void)
import           Control.Concurrent.MVar       (takeMVar, putMVar, newMVar)
import           System.IO

import           TxsAlex  (txsLexer)
import           TxsHappy (txsParser)
import           EnvCore  (IOC)
import           TxsCore  (txsSetCore, txsInitCore, txsTermitCore)
import           TxsStep  ( txsSetStep, txsShutStep
                          , txsStartStep, txsStopStep
                          , txsStepRun 
                          )
import           EnvData  (Msg)
import           TorXakis.Lens.TxsDefs (ix)
import           Name     (Name)
import           TxsDDefs (Verdict)

import qualified TxsServerConfig     as SC
import           TorXakis.Session

data Response = Success | Error { msg :: String } deriving (Show)

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

-- | Create a new session.
newSession :: IO Session 
newSession = do
    s <- Session <$> newTVarIO initSessionState
                 <*> newTQueueIO
                 <*> newMVar ()
                 <*> newTQueueIO
    uConfig <- SC.loadConfig
    case SC.interpretConfig uConfig of
      Left xs -> do
        hPutStrLn stderr "Errors found while loading the configuration"
        hPrint stderr xs
        return s
      Right config -> do
        resp <- runIOC s (txsSetCore config)
        case resp of
          Right _ -> return s
          Left  e -> do hPrint stderr e
                        return s

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
        Left err
          -> return $ Error $ show (err :: ErrorCall)
        Right _
          -> do st <- readTVarIO (s ^. sessionState)
                runIOC s $ do
                  resp <- txsInitCore (st ^. tdefs) (st ^. sigs) (msgHandler (_sessionMsgs s))
                  case resp of 
                    Right _ -> return Success
                    Left  e -> return $ Error e

unload :: Session -> IO Response
unload s = do
    -- update sessionState?
    resp <- runIOC s txsTermitCore
    case resp of
      Right _ -> return Success
      Left  e -> return $ Error e


-- | Start the stepper with the given model.
stepper :: Session
        -> Name        -- ^ Model name
        -> IO Response
stepper s mn =  do
    st <- readTVarIO (s ^. sessionState)
    runIOC s $ do
        -- Lookup the model definition that matches the name.
        let mMDef = st ^. tdefs . ix mn
        case mMDef of
            Nothing ->
                return $ Error $ "No model named " ++ show mn
            Just mDef -> do
                resp1 <- txsSetStep mDef
                resp2 <- txsStartStep
                case (resp1, resp2) of
                  (Right _, Right _) -> return Success
                  (Right _, Left e2) -> return $ Error e2
                  (Left e1, Right _) -> return $ Error e1
                  (Left e1, Left e2) -> return $ Error $ e1 ++ "\n" ++ e2

-- | Leave the stepper.
shutStepper :: Session -> IO Response
shutStepper s  =  do
    runIOC s $ do
        resp1 <- txsStopStep
        resp2 <- txsShutStep
        case (resp1, resp2) of
          (Right _, Right _) -> return Success
          (Right _, Left e2) -> return $ Error e2
          (Left e1, Right _) -> return $ Error e1
          (Left e1, Left e2) -> return $ Error $ e1 ++ "\n" ++ e2
    
msgHandler :: TQueue Msg -> [Msg] -> IOC ()
msgHandler q = lift . atomically . traverse_ (writeTQueue q)

-- | How a step is described
data StepType =  NumberOfSteps Int
               | ActionTxt String
    --           | GoTo StateNumber
    --           | Reset -- ^ Go to the initial state.
    --           | Rewind Steps
-- data StateNumber
-- data ActionName
-- TODO: discuss with Jan: do we need a `Tree` step here?

-- | Step for n-steps or actions
step :: Session -> StepType -> IO Response

step s (NumberOfSteps n) = do
    void $ forkIO $ runIOC s $ do
        resp <- txsStepRun n
        case resp of
          Right v -> do lift $ atomically $ writeTQueue (s ^. verdicts) v
                        return ()
          Left  _ -> return ()
    return Success

{-
 step :: Session -> StepType -> IO Response
 step s (NumberOfSteps n) = do
     void $ forkIO $ runIOC s $ do
         verdict <- txsStepN n
         lift $ atomically $ writeTQueue (s ^. verdicts) verdict
     return Success
 
-}


step _s (ActionTxt _txt) = do
    return $ Error "Step with actions not yet implemented: requires evaluation"

-- | Wait for a verdict to be reached.
waitForVerdict :: Session -> IO Verdict
waitForVerdict s = atomically $ readTQueue (s ^. verdicts)

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

