-- {-# LANGUAGE DeriveAnyClass  #-}
-- {-# LANGUAGE DeriveGeneric   #-}
-- {-# LANGUAGE TemplateHaskell #-}
module API
( startApp
, app
-- , User (..)
) where

import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Monad.Trans.Reader  (runReaderT)
-- import           Data.Aeson                  (FromJSON, ToJSON)
-- import           Data.Aeson.TH
import qualified Data.IntMap.Strict          as Map
-- import           Data.Swagger
-- import           GHC.Generics                (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
-- import           Servant.Server
-- import           Servant.Swagger

import           Common (TxsHandler, Env (..))
import           Endpoints.NewSession (NewSessionEP, newSrvSession)
import           Endpoints.Upload (UploadEP, upload)
-- import           Swagger

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show, Generic, ToSchema)

-- $(deriveJSON defaultOptions ''User)

type API = ServiceAPI
type ServiceAPI = NewSessionEP :<|> UploadEP
                            --    :<|> "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
    sessionsMap <- newTVarIO Map.empty
    zeroId      <- newTVarIO 0
    run 8080 $ app $ Env sessionsMap zeroId

app :: Env -> Application
app env = serve api $ hoistServer api (nt env) server
    where
        nt :: Env -> TxsHandler a -> Handler a
        nt env2 handler = runReaderT handler env2
        api :: Proxy API
        api = Proxy

server :: ServerT API TxsHandler
server = newSrvSession
    :<|> upload
    -- :<|> users
    -- :<|> return swaggerDocs
    -- where
    --     users :: TxsHandler [User]
    --     users = return [ User 1 "Isaac" "Newton"
    --                    , User 2 "Albert" "Einstein"
    --                    ]

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


