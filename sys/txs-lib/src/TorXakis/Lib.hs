{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}

-- |
module TorXakis.Lib where

import           Control.Arrow                 (left)
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.MVar       (newMVar, putMVar, takeMVar)
import           Control.Concurrent.STM.TChan  (newTChanIO)
import           Control.Concurrent.STM.TQueue (TQueue, isEmptyTQueue,
                                                newTQueueIO, readTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (modifyTVar', newTVarIO,
                                                readTVarIO)
import           Control.DeepSeq               (force)
import           Control.Exception             (ErrorCall, Exception,
                                                SomeException, catch, evaluate,
                                                toException, try)
import           Control.Monad                 (unless, void)
import           Control.Monad.Except          (ExceptT, liftEither, runExceptT,
                                                throwError)
import           Control.Monad.State           (lift, runStateT)
import           Control.Monad.STM             (atomically, retry)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Either.Utils             (maybeToEither)
import           Data.Foldable                 (traverse_)
import           Data.Map.Strict               as Map
import           Data.Semigroup                ((<>))
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (diffUTCTime, getCurrentTime,
                                                getCurrentTimeZone,
                                                utcToLocalTime)
import           GHC.Generics                  (Generic)
import           Lens.Micro                    ((&), (.~), (^.))

import qualified BuildInfo
import qualified VersionInfo

import           BehExprDefs                   (ChanOffer (Exclam, Quest),
                                                Offer (Offer))
import           ChanId                        (ChanId)
import           ConstDefs                     (Const)
import           EnvCore                       (IOC)
import qualified EnvCore                       as IOC
import           EnvData                       (Msg (TXS_CORE_SYSTEM_ERROR))
import           Name                          (Name)
import           ParamCore                     (getParamPairs, paramToPair,
                                                updateParam)
import           TorXakis.Lens.TxsDefs         (ix)
import           TxsAlex                       (Token (Cchanenv, Csigs, Cunid, Cvarenv),
                                                txsLexer)
import           TxsCore                       (txsEval, txsGetCurrentModel,
                                                txsGetSigs, txsGetTDefs,
                                                txsInitCore)
import           TxsDDefs                      (Action (Act),
                                                Verdict (NoVerdict))
import           TxsDefs                       (ModelDef (ModelDef))
import           TxsHappy                      (prefoffsParser, txsParser)
import           TxsStep                       (txsSetStep, txsShutStep,
                                                txsStartStep, txsStepAct,
                                                txsStepRun, txsStopStep)

import           TorXakis.Lib.Session

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

type Error = Text
type Response a = Either Error a

success :: Response ()
success = Right ()

newtype LibException = TxsError { errMsg :: Msg } deriving Show

instance Exception LibException

data TorXakisInfo = Info { version :: String, buildTime :: String }
    deriving (Generic)

instance ToJSON TorXakisInfo

info :: TorXakisInfo
info = Info VersionInfo.version BuildInfo.buildTime

newtype TimeResult = TimeResult { currentTime :: String }
    deriving (Generic)

instance ToJSON TimeResult
instance FromJSON TimeResult

time :: IO TimeResult
time = do
    tz  <- getCurrentTimeZone
    now <- getCurrentTime
    return $ TimeResult $ show $ utcToLocalTime tz now

-- | Create a new session.
newSession :: IO Session
newSession = Session <$> newTVarIO emptySessionState
                     <*> newTQueueIO
                     <*> newMVar ()
                     <*> newTQueueIO
                     <*> newTChanIO
                     <*> return (WorldConnDef Map.empty (\_ -> return []))
                     <*> return []
                     <*> newTVarIO Map.empty

-- | Stop a session.
killSession :: Session -> IO (Response ())
killSession _ =
    return $ Left "Kill Session: Not implemented (yet)"

data Timer = Timer { timerName :: String
                   , startTime :: String
                   , stopTime  :: String
                   , duration  :: String
                   }
    deriving (Generic)

instance ToJSON Timer
instance FromJSON Timer

timer :: Session -> String -> IO Timer
timer s nm = do
    tz  <- getCurrentTimeZone
    now <- getCurrentTime
    let timersT = s ^. timers
    timersMap <- readTVarIO timersT
    case Map.lookup nm timersMap of
        Nothing -> do
                    atomically $ modifyTVar' timersT $ Map.insert nm now
                    return $ Timer nm (show $ utcToLocalTime tz now) "" ""
        Just t  -> do
                    atomically $ modifyTVar' timersT $ Map.delete nm
                    return $ Timer nm
                                   (show $ utcToLocalTime tz t)
                                   (show $ utcToLocalTime tz now)
                                   (show $ diffUTCTime now t)

getAllParams :: Session -> [String] -> IO [(String, String)]
getAllParams s pNms = do
    cParams <- runIOC s $ IOC.getParams pNms
    st <- readTVarIO (s ^. sessionState)
    return $ cParams ++ getParamPairs pNms (st ^. sessionParams)

setParam :: Session -> String -> String -> IO (String,String)
setParam s pNm pVl = do
    setRes <- runIOC s $ IOC.setParams [(pNm, pVl)]
    case setRes of
        [] -> do
            let stT = s ^. sessionState
            st <- readTVarIO stT
            let params  = st ^. sessionParams
                params' = updateParam params (pNm, pVl)
                st'     = st & sessionParams .~ params'
                [pair]  = paramToPair params' pNm
            atomically $ modifyTVar' stT (const st')
            return pair
        [pair] -> return pair
        _ps    -> return ("","")

-- | Load a TorXakis file, compile it, and return the response.
--
-- The absolute path to the file is used to associate the parsed structure
-- (using the file-contents) with the file name, so that by calling `unload` we
-- know what to unload.
--
-- In the future we might want to make loading of 'TorXakis' models
-- incremental, to give better error messages, and support more modularity.
load :: Session -> FileContents -> IO (Response ())
load s xs = do
    r <- try $ do -- Since the 'TorXakis' parser currently just calls 'error'
                  -- we have to catch a generic 'ErrorCall' exception.
        (_, ts, is) <- evaluate . force . txsParser . txsLexer $ xs
        return (ts, is)
    case r of
        Left err -> return $ Left $ T.pack $ show  (err :: ErrorCall)
        Right (ts, is) -> runExceptT . runIOCE s $
            txsInitCore ts is (msgHandler (_sessionMsgs s))

-- | Set the stepper.
setStep :: Session
        -> Name -- ^ Model name
        -> IO (Response ())
setStep s mn = runResponse $ do
    mDef <- lookupModel s mn
    runIOCE s (txsSetStep mDef)

-- | Start the stepper. This step requires the stepper to be set. See
-- @setStep@.
startStep :: Session -> IO (Response ())
startStep s = runResponse $ runIOCE s txsStartStep

lookupModel :: Session -> Name -> ExceptT Text IO ModelDef
lookupModel s mn = do
    tdefs <- lift $ runIOC s txsGetTDefs
    maybe
        (throwError $ "No model named " <> mn)
        return (tdefs ^. ix mn)

-- | Leave the stepper.
shutStepper :: Session -> IO (Response ())
shutStepper s  = runResponse $ do
    runIOCE s txsStopStep
    runIOCE s txsShutStep

msgHandler :: TQueue Msg -> [Msg] -> IOC ()
msgHandler q = lift . atomically . traverse_ (writeTQueue q)

-- | How a step is described
--
data StepType = NumberOfSteps Int
              | AnAction Action
    --           | GoTo StateNumber
    --           | Reset -- ^ Go to the initial state.
    --           | Rewind Steps
              deriving (Show, Eq, Generic)

-- TODO: Types like 'StepType' are needed by the clients of 'txs-webserver'. So
-- to avoid introducing a dependency 'txs-lib' we could create a new package
-- called 'txs-lib-data', or something similar.

-- TODO: discuss with Jan: do we need a `Tree` step here?

instance ToJSON StepType
instance FromJSON StepType

-- | Step for n-steps or actions
step :: Session -> StepType -> IO (Response ())
step s (NumberOfSteps n) = runForVerdict s (txsStepRun n)
step s (AnAction a)      = runForVerdict s (txsStepAct a)

runForVerdict :: Session -> IOC (Either Msg Verdict) -> IO (Response ())
runForVerdict s ioc = do
    void $ forkIO $ do
        eVerd <- try $ runIOC s ioc
        case eVerd of
            Left e -> atomically $ writeTQueue (s ^. verdicts) (Left e)
            Right (Left eMsg) ->
                atomically $ writeTQueue (s ^. verdicts) $ Left $ toException $ TxsError eMsg
            Right (Right verd) ->
                atomically $ writeTQueue (s ^. verdicts) $ Right verd
    return success

-- | Wait for a verdict to be reached.
waitForVerdict :: Session -> IO (Either SomeException Verdict)
waitForVerdict s = atomically $ readTQueue (s ^. verdicts)

writeClosingVerdict :: Session -> IO ()
writeClosingVerdict s = atomically $ writeTQueue (s ^. verdicts) $ Right NoVerdict

-- | Wait for the message queue to be consumed.
waitForMessageQueue :: Session -> IO ()
waitForMessageQueue s = atomically $ do
    b <- isEmptyTQueue (s ^. sessionMsgs)
    unless b retry


runResponse :: ExceptT Text IO a -> IO (Response a)
runResponse = runExceptT

-- | Start the tester
tester :: Session
       -> Name
       -> IO (Response ())
tester s mn = runResponse $ do
    mDef <- lookupModel s mn
    lift $ do
        let fWCh = s ^. fromWorldChan
        tids <- (s ^. wConnDef . initWorld) fWCh
        -- let Just (deltaString,_) = Map.lookup "param_Sut_deltaTime" (st ^. prms)
        --     deltaTime = undefined -- read deltaString
        let s' = s & worldListeners .~ tids
        runIOC s' $ undefined mDef
            -- TODO: `t` this was commented out by Jan.
            -- txsSetTest (putToW fWCh (s' ^. wConnDef . toWorldMappings))
            --            (getFromW deltaTime fWCh)
            --            mDef Nothing Nothing

-- | Test for n-steps
test :: Session -> StepType -> IO (Response ())
test s (NumberOfSteps n) = do
    void $ forkIO $ do undefined s n
        -- TODO: `txsTestN` this was commented out by Jan.
        -- verdict <- try $ runIOC s $ txsTestN n
        -- atomically $ writeTQueue (s ^. verdicts) verdict
    return success
test s (AnAction a) = undefined s a

-- | Start the stepper with the given model.
stop :: Session -> IO (Response ())
stop _ =
    -- TODO: `txsStop` was commented out by Jan.
    -- runResponse $ lift $ runIOC s txsStop
    return success

-- | Parse a String into an set of offers.
--
parseAction :: Session -> Text -> IO (Response Action)
parseAction s act = do
    let strAct = T.unpack act
    sigs <- runIOC s txsGetSigs
    --
    -- IOS.Tested (TxsDefs.CnectDef _ conndefs) <- gets IOS.modus
    -- We don't have the connection definition here! Can we get this from `TxsDefs`?
    -- let chids = [ chan | TxsDefs.ConnDtoW chan _ _ _ _ <- conndefs ]
    --
    -- We don't have a local value in the session. Do we need one?
    --  vals <- gets IOS.locvals
    --                 , locvals :: TxsDefs.VEnv              -- ^ local value environment (EnvSever)
    --
    runExceptT $ do
        -- TODO: What is the correct way to get the `chids`?
        let
            cannotParse :: String
            cannotParse =  "There is no current model set, "
                        ++ "which is required for parsing an action"
        ModelDef is os _ _ <- runIOCE s $
            maybeToEither cannotParse <$> txsGetCurrentModel
        let chids = concatMap Set.toList is ++ concatMap Set.toList os
        parseRes <- fmap (left showEx) $
            lift $ try $ evaluate . force . prefoffsParser $
            ( Csigs    sigs
            : Cchanenv chids
            : Cvarenv  []
            : Cunid    0 -- Do we need to keep track of the UID in the session?
            : txsLexer strAct
            )
        (_, offs) <- liftEither parseRes
        cstOfs <- traverse offerToAction (Set.toList offs)
        return $ Act (Set.fromList cstOfs)
    where
      showEx :: SomeException -> Text
      showEx = T.pack . show
      offerToAction :: Offer -> ExceptT Text IO (ChanId, [Const])
      offerToAction (Offer cid offrs) = do
          csts <- traverse evalExclam  offrs
          return (cid, csts)

      evalExclam :: ChanOffer -> ExceptT Text IO Const
      evalExclam (Quest _) = throwError "No ? offer allowed as input."
      evalExclam (Exclam choff) = do
          res <- lift (runIOC s (txsEval choff))
          liftEither $ left T.pack res

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

-- | Run an IOC action but wrap the results in an exception.
runIOCE :: Show err => Session -> IOC (Either err a) -> ExceptT Text IO a
runIOCE s act = do
    er <- lift $ runIOC s act
    case er of
        Left eMsg -> throwError . T.pack . show $ eMsg
        Right res -> return res
