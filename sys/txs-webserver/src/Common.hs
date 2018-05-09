{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Common
    ( SessionId
    , Env (..)
    , getSession
    , getServerSession
    , getSessionIO
    , getServerSessionIO
    , ServerSession (..)
    )
where

import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString.Lazy.Char8  (pack)
import qualified Data.IntMap.Strict          as Map
import           Servant                     (throwError)
import           Servant.Server

import           TorXakis.Lib.Session        (Session)

type SessionId = Int

-- TODO: put this in the appropriate place. This needs to be done after all the
-- hs files in the webserver are put into subfolders with the 'TorXakis'
-- namespace.
data ServerSession = ServerSession
    { -- | TorXakis @Lib@ session.
      _libSession    :: Session
     -- | Continue listening for messages?
    , _contListening :: TVar Bool
    }

data Env = Env { sessions :: TVar (Map.IntMap ServerSession)
               , lastSid  :: TVar SessionId
               }

getServerSessionIO :: Env -> SessionId -> IO (Maybe ServerSession)
getServerSessionIO Env{sessions = ssT} sId = Map.lookup sId <$> readTVarIO ssT


getSessionIO :: Env -> SessionId -> IO (Maybe Session)
getSessionIO Env{sessions = ssT} sId = fmap _libSession . Map.lookup sId <$> readTVarIO ssT

getSession :: Env -> SessionId -> Handler Session
getSession env sId = do
    mSession <- liftIO $ getSessionIO env sId
    case mSession of
        Nothing ->
            throwError $ err422
                { errBody = pack $ "Session " ++ show sId ++ " not found." }
        Just s  ->
            return s

getServerSession :: Env -> SessionId -> Handler ServerSession
getServerSession env sId = do
    mSession <- liftIO $ getServerSessionIO env sId
    case mSession of
        Nothing ->
            throwError $ err422
                { errBody = pack $ "Session " ++ show sId ++ " not found." }
        Just s  ->
            return s
