{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
-- |
module TorXakis.Lib
( module TorXakis.Lib
, module TorXakis.Lib.Common
, module TorXakis.Lib.CommonCore
, module TorXakis.Lib.Eval
, module TorXakis.Lib.Session
, module TorXakis.Lib.Tester
, module TorXakis.Lib.Vals
, module TorXakis.Lib.Vars
)
where

import           Control.Arrow                 (left)
import           Control.Concurrent.MVar       (newMVar)
import           Control.Concurrent.STM.TChan  (newTChanIO)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue,
                                                writeTQueue)
import           Control.Concurrent.STM.TVar   (modifyTVar', newTVarIO,
                                                readTVarIO)
import           Control.DeepSeq               (force)
import           Control.Exception             (ErrorCall, Exception,
                                                SomeException, evaluate, try)
import           Control.Monad.Except          (ExceptT, liftEither, runExceptT,
                                                throwError)
import           Control.Monad.State           (lift)
import           Control.Monad.STM             (atomically)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Either.Utils             (maybeToEither)
import           Data.Foldable                 (traverse_)
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (diffUTCTime, getCurrentTime,
                                                getCurrentTimeZone,
                                                utcToLocalTime)
import           GHC.Generics                  (Generic)
import           Lens.Micro                    ((&), (.~), (^.))
import           System.Random                 (mkStdGen, setStdGen)

import qualified BuildInfo
import qualified VersionInfo

import           BehExprDefs                   (ChanOffer (Exclam, Quest),
                                                Offer (Offer))
import           ChanId                        (ChanId)
import           ConstDefs                     (Const)
import           EnvCore                       (IOC)
import qualified EnvCore                       as IOC
import           EnvData                       (Msg (TXS_CORE_SYSTEM_INFO))
import           Name                          (Name)
import           ParamCore                     (getParamPairs, paramToPair,
                                                updateParam)
import           TxsAlex                       (Token (Cchanenv, Csigs, Cunid, Cvarenv),
                                                txsLexer)
import qualified TxsCore                       as Core
import           TxsDDefs                      (Action (Act), Verdict)
import           TxsDefs                       (ModelDef (ModelDef))
import           TxsHappy                      (prefoffsParser, txsParser)

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Eval
import           TorXakis.Lib.Session
import           TorXakis.Lib.Tester
import           TorXakis.Lib.Vals
import           TorXakis.Lib.Vars

-- | For now file contents are represented as a string. This has to change in
-- the future, since it is quite inefficient, but we start off simple since the
-- current 'TorXakis' parser parses @String@s.
type FileContents = String

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
                     <*> newTVarIO (WorldConnDef Map.empty)
                     <*> newTVarIO []
                     <*> newTVarIO Map.empty
                     <*> newTVarIO []
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

setSeed :: Session -> Int -> IO ()
setSeed s seed = do
    setStdGen $ mkStdGen seed
    atomically $ writeTQueue (s ^. sessionMsgs) (TXS_CORE_SYSTEM_INFO $ "Global seed set to " ++ show seed)

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
        Right (ts, is) ->
            Right <$> runIOC s (Core.txsInit ts is (msgHandler (_sessionMsgs s)))
            -- case er of
            --     Left eMsg -> throwError . T.pack . show $ eMsg
            --     Right res -> return res

-- | Set the stepper.
setStep :: Session
        -> Name -- ^ Model name
        -> IO (Response ())
setStep s mn = runResponse $ do
    mDef <- lookupModel s mn
    lift $ runIOC s (Core.txsSetStep mDef)

msgHandler :: TQueue Msg -> [Msg] -> IOC ()
msgHandler q = lift . atomically . traverse_ (writeTQueue q)

-- | Step for n-steps or actions
step :: Session -> StepType -> IO (Response ())
step s (NumberOfSteps n) = runForVerdict s (Core.txsStepN n)
step s (AnAction a)      = runForVerdict s (Core.txsStepA a)

-- | Wait for a verdict to be reached.
waitForVerdict :: Session -> IO (Either SomeException Verdict)
waitForVerdict s = atomically $ readTQueue (s ^. verdicts)

-- | Start the stepper with the given model.
stop :: Session -> IO (Response ())
stop _ =
    -- TODO: `txsStop` was commented out by Jan.
    -- runResponse $ lift $ runIOC s txsStop
    return success

-- | Parse a String into a set of offers.
parseAction :: Session -> Text -> IO (Response Action)
parseAction s act = do
    let strAct = T.unpack act
    sigs <- runIOC s Core.txsGetSigs
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
            maybeToEither cannotParse <$> Core.txsGetCurrentModel
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
      offerToAction :: Offer -> ExceptT Text IO (ChanId, [Const])
      offerToAction (Offer cid offrs) = do
          csts <- traverse evalExclam  offrs
          return (cid, csts)

      evalExclam :: ChanOffer -> ExceptT Text IO Const
      evalExclam (Quest _) = throwError "No ? offer allowed as input."
      evalExclam (Exclam choff) = do
          res <- lift (runIOC s (Core.txsEval choff))
          liftEither $ left T.pack res
