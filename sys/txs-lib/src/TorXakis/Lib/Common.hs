{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- |
module TorXakis.Lib.Common where

import           Control.Concurrent.MVar       (putMVar, takeMVar)
import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar   (modifyTVar', readTVarIO)
import           Control.Exception             (SomeException, catch)
import           Control.Monad.Except          (ExceptT, throwError)
import           Control.Monad.State           (lift, runStateT)
import           Control.Monad.STM             (atomically)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Lens.Micro                    ((.~), (^.))

import           EnvCore                       (IOC)
import           EnvData                       (Msg (TXS_CORE_SYSTEM_ERROR))

import           TorXakis.Lib.Session

type Error = Text
type Response a = Either Error a

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

-- | Show exception as `Text`
showEx :: SomeException -> Text
showEx = T.pack . show
