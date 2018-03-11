module Endpoints.NewSession
    ( NewSessionEP
    , newSrvSession
    ) where

import           Control.Concurrent.STM.TVar (modifyTVar, readTVarIO, writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ask)
import qualified Data.IntMap.Strict          as Map
import           Servant

import           TorXakis.Lib     (newSession)

import           Common (SessionId, TxsHandler, Env(..))

type NewSessionEP = "session" :> "new" :> PostCreated '[JSON] SessionId

newSrvSession :: TxsHandler SessionId
newSrvSession = do
    Env{sessions = ssT, lastSid = pSidT} <- ask
    pSid       <- liftIO $ readTVarIO pSidT
    s <- liftIO newSession
    let sid = pSid + 1
    liftIO $ atomically $ modifyTVar ssT   (Map.insert sid s)
    liftIO $ atomically $ writeTVar  pSidT sid
    return sid
