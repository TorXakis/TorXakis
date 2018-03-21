{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module API
( startApp
, app
, User (..)
) where

import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Monad.Trans.Reader  (runReaderT)
-- import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Aeson.TH
import qualified Data.IntMap.Strict          as Map
-- import           Data.Swagger
import           GHC.Generics                (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
-- import           Servant.Server
-- import           Servant.Swagger

import           Common                      (Env (..))
import           Endpoints.Messages          (MessagesEP, streamMessages, SSMessagesEP, ssMessagesEP)
import           Endpoints.NewSession        (NewSessionEP, newSrvSession)
import           Endpoints.Stepper           (StartStepperEP, TakeNStepsEP,
                                              startStepper, takeNSteps)
import           Endpoints.Upload            (UploadEP, upload)
-- import           Swagger

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''User)

type API = ServiceAPI
type ServiceAPI = NewSessionEP :<|> UploadEP :<|> StartStepperEP :<|> TakeNStepsEP :<|> MessagesEP
                               :<|> SSMessagesEP
                               :<|> "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
    sessionsMap <- newTVarIO Map.empty
    zeroId      <- newTVarIO 0
    run 8080 $ app $ Env sessionsMap zeroId

app :: Env -> Application
app env = simpleCors $ serve api (server env)
    where
        api :: Proxy API
        api = Proxy

server :: Env -> ServerT API Handler
server env = newSrvSession env 
    :<|> upload env 
    :<|> startStepper env
    :<|> takeNSteps env
    :<|> streamMessages env
    :<|> ssMessagesEP env
    :<|> users
    -- :<|> return swaggerDocs
    where
        users :: Handler [User]
        users = return [ User 1 "Isaac" "Newton"
                       , User 2 "Albert" "Einstein"
                       ]

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


