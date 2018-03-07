{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Map.Strict
import Data.Swagger
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic, ToSchema)

$(deriveJSON defaultOptions ''User)

type API = SwaggerAPI :<|> ServiceAPI

type ServiceAPI = "users" :> Get '[JSON] [User] :<|> NewSessionEP
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return swaggerDocs :<|> return users :<|> return newSession

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

swaggerDocs :: Swagger
swaggerDocs = toSwagger serviceAPI
    where serviceAPI :: Proxy ServiceAPI
          serviceAPI = Proxy

type SessionId = Int
type NewSessionEP = "session" :> "new" :> PostCreated '[JSON] SessionId

newSession :: SessionId
newSession = undefined



-- loadModel :: IO ()
-- loadModel = undefined

-- loadSession :: SessionId -> IO ()
-- loadSession sid = undefined

-- startStepper :: SessionId -> Text -> IO ()
-- startStepper sid modelNm = undefined

-- step :: SessionId -> Int -> IO ()
-- step sid steps = undefined



