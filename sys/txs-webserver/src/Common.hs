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
    , checkResult
    , liftLib
    )
where

import           Control.Arrow               ((|||))
import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString.Lazy.Char8  (pack)
import qualified Data.IntMap.Strict          as Map
import qualified Data.Text.Lazy              as TL
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Servant                     (throwError)
import           Servant.Server

import           TorXakis.Lib                (Error, Response)
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

-- | Check the result and throw a 400 error if it is a left.
checkResult :: Either Error a -> Handler a
checkResult = (\e -> throwError err400 { errBody = encodeUtf8 (TL.fromStrict e) }) ||| return

-- | Call a @Lib@ function using the @Session@ associated to the given session
-- id. The result @Either Error a@ of the @Lib@ function will be converted to a
-- @Handler a@ value. Left values will be converted to 400 error codes.
liftLib :: Env -> SessionId -> (Session -> IO (Response a)) -> Handler a
liftLib env sId f = do
    s <- getSession env sId
    r <- liftIO (f s)
    checkResult r
