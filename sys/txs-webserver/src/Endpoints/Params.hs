{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric     #-}
module Endpoints.Params
    ( ParamEP
    , param
    , ParamsEP
    , params
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (ToJSON)
import           GHC.Generics           (Generic)
import           Servant

import qualified TorXakis.Lib           as Lib

import           Common                 (Env, SessionId, getSession)

data ParameterValue = ParameterValue { _paramName  :: String
                                     , _paramValue :: String
                                     }
    deriving (Generic)

instance ToJSON ParameterValue

type ParamEP = "sessions"
             :> Capture "sid" SessionId
             :> "params"
             :> Capture "pNm" String
             :> Get '[JSON] ParameterValue

param :: Env -> SessionId -> String -> Handler ParameterValue
param env sid pNm = do
    s <- getSession env sid
    [prm] <- liftIO $ Lib.getAllParams s [pNm]
    return $ showPrm prm

type ParamsEP = "sessions"
             :> Capture "sid" SessionId
             :> "params"
             :> Get '[JSON] [ParameterValue]

params :: Env -> SessionId -> Handler [ParameterValue]
params env sid = do
    s <- getSession env sid
    prms <- liftIO $ Lib.getAllParams s []
    return $ map showPrm prms

showPrm :: (String,String) -> ParameterValue
showPrm (nm,vl) = ParameterValue nm vl
