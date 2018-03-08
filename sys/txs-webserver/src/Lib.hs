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
import qualified Data.ByteString.Lazy        as LBS
import           Data.Foldable
import qualified Data.Map.Strict             as Map
import           Data.Swagger
import           GHC.Generics                (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server
import           Servant.Multipart           (MultipartForm, MultipartData, Mem, iName, iValue, inputs, files, fdPayload, fdFileName)
-- import           Servant.Swagger

import           TorXakis.Lib     (newSession)
import           TorXakis.Session (Session)
-- import           Swagger

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic, ToSchema)

$(deriveJSON defaultOptions ''User)

type API = ServiceAPI
type ServiceAPI = "users" :> Get '[JSON] [User] :<|> NewSessionEP :<|> UploadEP
type NewSessionEP = "session" :> "new" :> PostCreated '[JSON] SessionId
type UploadEP = "session" :> Capture "sid" SessionId :> "model" :> MultipartForm Mem (MultipartData Mem) :> PostCreated '[JSON] Integer
type SessionId = Int
newtype Env = Env {sessions :: TVar (Map.Map SessionId Session)}

startApp :: IO ()
startApp = do
    sessionsMap <- newTVarIO Map.empty
    run 8080 $ app $ Env sessionsMap

app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server
    where
        nt :: Env -> TxsHandler a -> Handler a
        nt env handler = runReaderT handler env
        api :: Proxy API
        api = Proxy

type TxsHandler = ReaderT Env Handler

server :: ServerT API TxsHandler
server = users
    :<|> newSrvSession
    :<|> upload
    -- :<|> return swaggerDocs
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
        upload :: SessionId -> MultipartData Mem -> TxsHandler Integer
        upload sid multipartData =  do
            liftIO $ do
              putStrLn "Inputs:"
              forM_ (inputs multipartData) $ \input ->
                putStrLn $ "  " ++ show (iName input)
                      ++ " -> " ++ show (iValue input)
          
              forM_ (files multipartData) $ \file -> do
                let content = fdPayload file
                putStrLn $ "Content of " ++ show (fdFileName file)
                LBS.putStr content
            return 0

        -- swaggerDocs :: Swagger
        -- swaggerDocs = toSwagger serviceAPI
        --     where serviceAPI :: Proxy ServiceAPI
        --           serviceAPI = Proxy


-- loadModel :: IO ()
-- loadModel = undefined

-- loadSession :: SessionId -> IO ()
-- loadSession sid = undefined

-- startStepper :: SessionId -> Text -> IO ()
-- startStepper sid modelNm = undefined

-- step :: SessionId -> Int -> IO ()
-- step sid steps = undefined


