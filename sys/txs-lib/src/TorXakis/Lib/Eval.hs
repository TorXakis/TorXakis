{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Eval where

import           Control.Monad.Except    (runExceptT)
import           Data.Text               (Text)

import           TxsCore                 (txsEval)
import           TxsShow                 (fshow)

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session

eval :: Session -> Text -> IO (Response String)
eval s expr = do
    psRes <- parseVexprAndSubst s expr
    case psRes of
        Right psExp -> do
            res <- runExceptT $ runIOCE s $ txsEval psExp
            case res of
                Right evalRes -> return $ Right $ fshow evalRes
                Left err      -> return $ Left err
        Left  err   -> return $ Left err
