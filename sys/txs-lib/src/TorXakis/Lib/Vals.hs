{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
-- |
module TorXakis.Lib.Vals where

import           Control.Arrow               (left)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVarIO)
import           Control.DeepSeq             (force)
import           Control.Exception           (evaluate, try)
import           Control.Monad.Except        (liftEither, runExceptT,
                                              throwError)
import           Control.Monad.State         (lift)
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  (ToJSON)
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import           Lens.Micro                  ((^.))

import           TxsAlex                     (Token (Csigs, Cunid, Cvarenv),
                                              txsLexer)
import           TxsCore                     (txsGetSigs)
import           TxsHappy                    (valdefsParser)
import           TxsShow                     (fshow)
import           ValExpr                     (ValExpr)
import qualified VarId

import           TorXakis.Lib.Common
import           TorXakis.Lib.Session

data Val = Val { _valName :: String
               , _valExpr :: String
               }
    deriving (Generic, Show)

instance ToJSON Val

createVal :: Session -> Text -> IO (Response Val)
createVal s val = do
    let strVal = T.unpack val
        varsT  = s ^. locVars
        valEnvT = s ^. locValEnv
    valEnv <- readTVarIO valEnvT
    if T.null val
        then return $ Left "No value expression received"
        else do
            vars <- readTVarIO varsT
            sigs <- runIOC s txsGetSigs
            runExceptT $ do
                parseRes <- fmap (left showEx) $
                    lift $ try $ evaluate . force . valdefsParser $
                    ( Csigs    sigs
                    : Cvarenv  []
                    : Cunid    0
                    : txsLexer strVal
                    )
                (_, venv) <- liftEither parseRes
                if let newnames = map VarId.name (Map.keys venv)
                    in null (newnames `List.intersect` map VarId.name vars) &&
                       null (newnames `List.intersect` map VarId.name (Map.keys valEnv))
                  then lift $ atomically $ modifyTVar' valEnvT $ Map.union venv
                  else throwError $ T.pack $ "double value names: " ++ fshow venv
                return $ head $ toVals venv

getVals :: Session -> IO (Response [Val])
getVals s = do
    let valEnvT = s ^. locValEnv
    valEnv <- readTVarIO valEnvT
    runExceptT $ return $ toVals valEnv

toVals :: Map.Map VarId.VarId (ValExpr VarId.VarId) -> [Val]
toVals venv =   [ Val (T.unpack $ VarId.name vid) (fshow vexp)
                | (vid,vexp) <- Map.toList venv
                ]
