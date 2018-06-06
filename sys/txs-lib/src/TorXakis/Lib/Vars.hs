{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
-- |
module TorXakis.Lib.Vars where

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

import           Id                          (_id)
import qualified SortId
import           TxsAlex                     (Token (Csigs, Cunid), txsLexer)
import           TxsCore                     (txsGetSigs)
import           TxsHappy                    (vardeclsParser)
import           TxsShow                     (fshow)
import           Variable                    (vsort, vunid)
import qualified VarId

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session

data Var = Var { _varName     :: Text
               , _varUnId     :: Int
               , _varSortName :: Text
               , _varSortUnId :: Int
               }
    deriving (Generic, Show)

instance ToJSON Var

createVar :: Session -> Text -> IO (Response Var)
createVar s var = do
    let strVar = T.unpack var
        varsT  = s ^. locVars
        valEnvT = s ^. locValEnv
    if T.null var
        then return $ Left "No variable declaration received"
        else do
            valEnv <- readTVarIO valEnvT
            vars <- readTVarIO varsT
            sigs <- runIOC s txsGetSigs
            runExceptT $ do
                parseRes <- fmap (left showEx) $
                    lift $ try $ evaluate . force . vardeclsParser $
                    ( Csigs    sigs
                    : Cunid    0
                    : txsLexer strVar
                    )
                (_, newVars) <- liftEither parseRes
                if let newnames = map VarId.name newVars
                    in null (newnames `List.intersect` map VarId.name vars) &&
                       null (newnames `List.intersect` map VarId.name (Map.keys valEnv))
                  then lift $ atomically $ modifyTVar' varsT (++ newVars)
                  else throwError $ T.pack $ "double variable names: " ++ fshow newVars
                return $ head $ map toVar newVars

getVars :: Session -> IO (Response [Var])
getVars s = do
    let varsT = s ^. locVars
    vars <- readTVarIO varsT
    runExceptT $ return $ map toVar vars

toVar :: VarId.VarId -> Var
toVar varId = Var (VarId.name varId)
                  (vunid varId)
                  (SortId.name vSort)
                  (_id $ SortId.unid vSort)
            where vSort = vsort varId
