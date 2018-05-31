{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Eval where

import           Control.Arrow               (left)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.DeepSeq             (force)
import           Control.Exception           (evaluate, try)
import           Control.Monad.Except        (liftEither, runExceptT)
import           Control.Monad.State         (lift)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro                  ((^.))

import           TxsAlex                     (Token (Csigs, Cunid, Cvarenv),
                                              txsLexer)
import           TxsCore                     (txsEval, txsGetSigs, txsGetTDefs)
import           TxsDefs                     (funcDefs)
import           TxsHappy                    (vexprParser)
import           TxsShow                     (fshow)
import           ValExpr                     (subst)

import           TorXakis.Lib.Common
import           TorXakis.Lib.Session

eval :: Session -> Text -> IO (Response String)
eval s expr = do
    let strExp = T.unpack expr
        varsT  = s ^. locVars
        valEnvT = s ^. locValEnv
    if T.null expr
        then return $ Left "No value expression received"
        else do
            valEnv <- readTVarIO valEnvT
            vars <- readTVarIO varsT
            sigs <- runIOC s txsGetSigs
            tDefs <- runIOC s txsGetTDefs
            runExceptT $ do
                parseRes <- fmap (left showEx) $
                    lift $ try $ evaluate . force . vexprParser $
                    ( Csigs    sigs
                    : Cvarenv  (Map.keys valEnv ++ vars)
                    : Cunid    0
                    : txsLexer strExp
                    )
                (_, vexp) <- liftEither parseRes
                res <- runIOCE s $ txsEval (subst valEnv (TxsDefs.funcDefs tDefs) vexp)
                return $ fshow res
