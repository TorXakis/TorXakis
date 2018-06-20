{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Solvers
( solve
, unisolve
, ransolve
)
where

import           Data.Map.Strict         (toList)
import           Data.Text               (Text)

import           TxsCore                 (TxsSolveType, txsRanSolve, txsSolve,
                                          txsUniSolve)
import           TxsShow                 (fshow)

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session

solve :: Session -> Text -> IO (Response String)
solve = solveWith txsSolve

unisolve :: Session -> Text -> IO (Response String)
unisolve = solveWith txsUniSolve

ransolve :: Session -> Text -> IO (Response String)
ransolve = solveWith txsRanSolve

solveWith :: TxsSolveType -> Session -> Text -> IO (Response String)
solveWith solver s expr = do
    psRes <- parseVexprAndSubst s expr
    case psRes of
        Right psExp -> do
            res <- runIOC s $ solver psExp
            return $ Right $ fshow $ toList res
        Left  err   -> return $ Left err
