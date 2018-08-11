{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-----------------------------------------------------------------------------
-- |
-- Module      :  TxsValue
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- API for TorXakis core functionality.
-----------------------------------------------------------------------------
-- {-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}


module TxsValue
(
  -- * evaluation of value expression
, txsEval

  -- * Solving
  -- ** Type of solver
  --, TxsSolveType

  -- ** finding a solution for value expression
, txsSolve

  -- ** finding an unique solution for value expression
, txsUniSolve

  -- ** finding a random solution for value expression
, txsRanSolve

)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import           Control.Arrow
-- import           Control.Monad
-- import           Control.Monad.State
-- import qualified Data.List           as List
-- import qualified Data.Map            as Map
-- import           Data.Maybe
-- import           Data.Monoid
-- import qualified Data.Set            as Set
-- import qualified Data.Text           as T
-- import           System.IO
-- import           System.Random

-- -- import from local
-- import           CoreUtils
-- import           Ioco
-- import           Mapper
-- import           NComp
-- import           Purpose
-- 
-- import           Config              (Config)
-- import qualified Config

-- -- import from bexpr
-- import           Relabel             (relabel)
-- 
-- -- import from behave(defs)
-- import qualified Behave
-- import qualified BTree
-- 
-- -- import from coreenv
-- import qualified EnvCore             as IOC
-- import qualified EnvData
-- import qualified ParamCore
-- 
-- -- import from defs
-- import qualified Sigs
-- import qualified TxsDDefs
-- import qualified TxsDefs
-- import qualified TxsShow
-- import           TxsUtils
-- 
-- -- import from solve
-- import qualified FreeVar
-- import qualified SMT
-- import qualified Solve
-- import qualified Solve.Params
-- import qualified SolveDefs
-- import qualified SMTData
-- 
-- -- import from value
-- import qualified Eval
-- 
-- -- import from lpe
-- -- import qualified LPE
-- import qualified LPE
-- 
-- -- import from valexpr
-- import qualified SortId
-- import qualified SortOf
-- import Constant
-- import VarId


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --


-- | Evaluate the provided value expression.
--
--   Only possible when txscore is initialized.
txsEval :: TxsDefs.VExpr                    -- ^ value expression to be evaluated.
        -> IOC.IOC (Either String Constant)
txsEval vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No 'eval' without model" ]
               return $ Left "No 'eval' without model"
       _ -> let frees = FreeVar.freeVars vexp
            in if  not $ null frees
                     then do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                           $ "Value expression not closed: " ++
                                             TxsShow.fshow frees ]
                             return $ Left $ "Value expression not closed: " ++
                                             TxsShow.fshow frees
                     else do envb         <- filterEnvCtoEnvB
                             (wal',envb') <- lift $ runStateT (Eval.eval vexp) envb
                             writeEnvBtoEnvC envb'
                             return wal'

-- | Type for solve (@txsSolve, @txsUniSolve, and @txsRanSolve)                             
type TxsSolveType = TxsDefs.VExpr                   -- ^ value expression to solve.
                    -> IOC.IOC (TxsDefs.WEnv VarId)
                        
-- | Find a solution for the provided Boolean value expression.
--
--   Only possible when txscore is initialized.
txsSolve :: TxsSolveType
txsSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR  "No 'solve' without model" ]
               return Map.empty
       _ -> if  SortOf.sortOf vexp /= SortId.sortIdBool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                 "Value expression for solve shall be Bool" ]
                   return Map.empty
                 else do
                   let frees = FreeVar.freeVars vexp
                       assertions = Solve.add vexp Solve.empty
                   smtEnv        <- IOC.getSMT "current"
                   (sat,smtEnv') <- lift $ runStateT (Solve.solve frees assertions) smtEnv
                   IOC.putSMT "current" smtEnv'
                   case sat of
                     SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "sat" ]
                                                   return sol
                     SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unsat" ]
                                                   return Map.empty
                     SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unknown" ]
                                                   return Map.empty


-- | Find an unique solution for the provided Boolean value expression.
--
--   Only possible when txscore is initialized.
txsUniSolve :: TxsSolveType
txsUniSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No 'solve' without model" ]
               return Map.empty
       _ -> if  SortOf.sortOf vexp /= SortId.sortIdBool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression shall be Bool" ]
                   return Map.empty
                 else do
                   let frees = FreeVar.freeVars vexp
                       assertions = Solve.add vexp Solve.empty
                   smtEnv        <- IOC.getSMT "current"
                   (sat,smtEnv') <- lift $ runStateT (Solve.uniSolve frees assertions) smtEnv
                   IOC.putSMT "current" smtEnv'
                   case sat of
                     SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "sat" ]
                                                   return sol
                     SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unsat" ]
                                                   return Map.empty
                     SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unknown" ]
                                                   return Map.empty

-- | Find a random solution for the provided Boolean value expression.
--
--   Only possible when txscore is initialized.
txsRanSolve :: TxsSolveType
txsRanSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No 'solve' without model" ]
               return Map.empty
       _ -> if  SortOf.sortOf vexp /= SortId.sortIdBool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression shall be Bool" ]
                   return Map.empty
                else do
                   let frees      = FreeVar.freeVars vexp
                       assertions = Solve.add vexp Solve.empty
                   smtEnv        <- IOC.getSMT "current"
                   parammap <- gets IOC.params
                   let p = Solve.toRandParam parammap
                   (sat,smtEnv') <- lift $ runStateT (Solve.randSolve p frees assertions) smtEnv
                   IOC.putSMT "current" smtEnv'
                   case sat of
                     SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "sat" ]
                                                   return sol
                     SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unsat" ]
                                                   return Map.empty
                     SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unknown" ]
                                                   return Map.empty


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

