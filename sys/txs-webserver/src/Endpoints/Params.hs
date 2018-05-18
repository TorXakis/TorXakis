{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module Endpoints.Params
    ( ParamsEP
    , params
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Servant

import qualified TorXakis.Lib           as Lib

import           Common                 (Env, SessionId, getSession)

type ParamsEP = "sessions"
             :> Capture "sid" SessionId
             :> "params"
             :> Capture "pNms" String
             :> Post '[JSON] String

params :: Env -> SessionId -> String -> Handler String
params env sid pNms = do
    s <- getSession env sid
    prms <- liftIO $ Lib.getCoreAndSolveParams s $ words pNms
    return $ unlines $ map displayPrm prms
      where
        displayPrm :: (String,String) -> String
        displayPrm (nm,vl) = concat [nm, " = ", vl]

