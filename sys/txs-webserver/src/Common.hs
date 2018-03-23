{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Common
(
  SessionId
, Env (..)
, getSession
, getSessionIO
)
where

import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString.Lazy.Char8  (pack)
import qualified Data.IntMap.Strict          as Map
import           Servant                     (throwError)
import           Servant.Server

import           TorXakis.Session            (Session)

type SessionId = Int

data Env = Env { sessions :: TVar (Map.IntMap Session)
               , lastSid  :: TVar SessionId
               }

getSessionIO :: Env -> SessionId -> IO  (Maybe Session)
getSessionIO Env{sessions = ssT} sid = Map.lookup sid <$> readTVarIO ssT

getSession :: Env -> SessionId -> Handler Session
getSession e sId = do
    mSession <- liftIO $ getSessionIO e sId
    case mSession of
        Nothing -> throwError $ err422 { errBody = pack $ "Session " ++ show sId ++ " not found." }
        Just s  -> return s
