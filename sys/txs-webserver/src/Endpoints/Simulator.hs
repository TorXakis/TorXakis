
{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoints.Simulator where

import           Servant
import           Servant.Multipart (Mem, MultipartData, MultipartForm, iName,
                                    iValue, inputs)

import           TorXakis.Lib      (StepType (..), setSim, sim)

import           Common            (Env, SessionId, liftLib)

type SetSimEP = "sessions"
              :> Capture "sid" SessionId
              :> "set-sim"
              :> MultipartForm Mem (MultipartData Mem)
              :> PostNoContent '[JSON] ()

startSimulator :: Env -> SessionId -> MultipartData Mem -> Handler ()
startSimulator env sId mpData = do
    let is = inputs mpData
    if length is /= 3
        then throwError err400 { errBody = "Invalid arguments to set-sim." }
        else do
            let (m,c,mp) = foldr extractName ("","","") is
            liftLib env sId $ setSim m c mp
  where
    extractName inp (m,c,mp) =
        case iName inp of
            "model" -> (iValue inp, c, mp)
            "cnect" -> (m, iValue inp, mp)
            "map"   -> (m, c, iValue inp)
            _       -> (m, c, mp)

type SimStepEP = "sessions"
           :> Capture "sid" SessionId
           :> "sim"
           :> ReqBody '[JSON] StepType
           :> PostNoContent '[JSON] ()

simStep :: Env -> SessionId -> StepType -> Handler ()
simStep env sId sType = liftLib env sId (`sim` sType)
