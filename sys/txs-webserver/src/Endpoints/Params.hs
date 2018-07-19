{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
module Endpoints.Params
    ( ParamsAPI
    , paramsServer
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (ToJSON)
import qualified Data.Text              as T
import           GHC.Generics           (Generic)
import           Servant
import           Servant.Multipart      (Mem, MultipartData, MultipartForm,
                                         iName, iValue, inputs)

import qualified TorXakis.Lib           as Lib

import           Common                 (Env, SessionId, getSession)

data ParameterValue = ParameterValue { _paramName  :: String
                                     , _paramValue :: String
                                     }
    deriving (Generic)

instance ToJSON ParameterValue

type ParamsAPI = "sessions"
                :> Capture "sid" SessionId
                :> "params"
                :>
                (
                    Capture "pNm" String :> Get '[JSON] ParameterValue
                :<|>
                    Get '[JSON] [ParameterValue]
                :<|>
                    MultipartForm Mem (MultipartData Mem) :> Put '[JSON] ParameterValue
                )

paramsServer :: Env -> Server ParamsAPI
paramsServer env sid = param :<|> params :<|> setParam
    where
        param :: String -> Handler ParameterValue
        param pNm = do
            s <- getSession env sid
            [prm] <- liftIO $ Lib.getAllParams s [pNm] -- TODO: this might result in an error!
            return $ showPrm prm
        params :: Handler [ParameterValue]
        params = do
            s <- getSession env sid
            prms <- liftIO $ Lib.getAllParams s []
            return $ map showPrm prms
        setParam :: MultipartData Mem -> Handler ParameterValue
        setParam mpData = do
            s <- getSession env sid
            case inputs mpData of
                [pIn] -> do
                    prm <- liftIO $ Lib.setParam s (T.unpack $ iName pIn) (T.unpack $ iValue pIn)
                    return $ showPrm prm
                _     -> return $ ParameterValue "" ""
        showPrm :: (String,String) -> ParameterValue
        showPrm (nm,vl) = ParameterValue nm vl
