{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Params where

import           Control.Concurrent.STM.TVar (modifyTVar', readTVarIO)
import           Control.Monad.STM           (atomically)
import           Lens.Micro                  ((&), (.~), (^.))

import qualified EnvCore                     as IOC
import           ParamCore                   (getParamPairs, paramToPair,
                                              updateParam)

import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session

getAllParams :: Session -> [String] -> IO [(String, String)]
getAllParams s pNms = do
    cParams <- runIOC s $ IOC.getParams pNms
    st <- readTVarIO (s ^. sessionState)
    return $ cParams ++ getParamPairs pNms (st ^. sessionParams)

setParam :: Session -> String -> String -> IO (String,String)
setParam s pNm pVl = do
    setRes <- runIOC s $ IOC.setParams [(pNm, pVl)]
    case setRes of
        [] -> do
            let stT = s ^. sessionState
            st <- readTVarIO stT
            let params  = st ^. sessionParams
                params' = updateParam params (pNm, pVl)
                st'     = st & sessionParams .~ params'
                [pair]  = paramToPair params' pNm
            atomically $ modifyTVar' stT (const st')
            return pair
        [pair] -> return pair
        _ps    -> return ("","")
