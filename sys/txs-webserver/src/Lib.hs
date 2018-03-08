{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where


import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, modifyTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Aeson.TH
import qualified Data.Map.Strict             as Map
import           Data.Swagger
import           GHC.Generics                (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server
-- import Servant.Multipart
import           Servant.Swagger

import           TorXakis.Lib     (newSession)
import           TorXakis.Session (Session)
import           Swagger

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic, ToSchema)

$(deriveJSON defaultOptions ''User)

type API = SwaggerAPI :<|> ServiceAPI

type ServiceAPI = "users" :> Get '[JSON] [User] :<|> NewSessionEP

startApp :: IO ()
startApp = do
    sessionsMap <- newTVarIO Map.empty
    run 8080 $ app $ Env sessionsMap

app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server

api :: Proxy API
api = Proxy

server :: ServerT API TxsHandler
server = return swaggerDocs
        :<|> users
        :<|> newSrvSession
    where
        users :: TxsHandler [User]
        users = return [ User 1 "Isaac" "Newton"
                       , User 2 "Albert" "Einstein"
                       ]
        newSrvSession :: TxsHandler SessionId
        newSrvSession = do
            Env{sessions = ssT} <- ask
            s <- liftIO newSession
            let sid = 42 -- TODO:
            liftIO $ atomically $ modifyTVar ssT (Map.insert sid s)
            return sid


nt :: Env -> TxsHandler a -> Handler a
nt env handler = runReaderT handler env

type SessionId = Int
newtype Env = Env {sessions :: TVar (Map.Map SessionId Session)}

type TxsHandler = ReaderT Env Handler

type NewSessionEP = "session" :> "new" :> PostCreated '[JSON] SessionId

-- loadModel :: IO ()
-- loadModel = undefined

-- loadSession :: SessionId -> IO ()
-- loadSession sid = undefined

-- startStepper :: SessionId -> Text -> IO ()
-- startStepper sid modelNm = undefined

-- step :: SessionId -> Int -> IO ()
-- step sid steps = undefined


swaggerDocs :: Swagger
swaggerDocs = toSwagger serviceAPI
    where serviceAPI :: Proxy ServiceAPI
          serviceAPI = Proxy

