module Common
(
  SessionId
, TxsHandler
, Env (..)
, getSession
)
where

import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Reader  (ask, ReaderT)
import           Data.ByteString.Lazy.Char8  (pack)
import qualified Data.IntMap.Strict          as Map
import           Servant                     (throwError)
import           Servant.Server

import           TorXakis.Session (Session)

type SessionId = Int

type TxsHandler = ReaderT Env Handler
data Env = Env { sessions :: TVar (Map.IntMap Session)
               , lastSid  :: TVar SessionId
               }

getSession :: SessionId -> TxsHandler Session
getSession sid = do
    Env{sessions = ssT} <- ask
    sessionsMap <- liftIO $ readTVarIO ssT
    case Map.lookup sid sessionsMap of
        Nothing -> throwError $ err422 { errBody = pack $ "Session " ++ show sid ++ " not found." }
        Just s  -> return s