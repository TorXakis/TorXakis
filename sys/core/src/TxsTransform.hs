{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TxsTransform
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- Transformations on internal structures.
-----------------------------------------------------------------------------

-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ViewPatterns        #-}
module TxsCore

(
  -- ** test purposes for n-complete coverage
, txsNComplete   -- :: TxsDefs.ModelDef -> IOC.IOC (Either EnvData.Msg TxsDefs.PurpId)

  -- ** LPE transformation
, txsLPE         -- :: Either TxsDefs.BExpr TxsDefs.ModelId
                 -- -> IOC.IOC (Either EnvData.Msg (Either TxsDefs.BExpr TxsDefs.ModelId))

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

-- import from local
-- import           CoreUtils
-- import           Ioco
-- import           Mapper
-- import           NComp
-- import           Purpose
-- import           Sim
-- import           Step
-- import           Test

-- import           Config              (Config)
-- import qualified Config

-- import from behave(defs)
-- import qualified Behave
-- import qualified BTree
-- import           Expand              (relabel)

-- import from coreenv
-- import           EnvCore             (modeldef)
-- import qualified EnvCore             as IOC
-- import qualified EnvData
-- import qualified ParamCore

-- import from defs
-- import qualified Sigs
-- -- import qualified TxsDDefs
-- import qualified TxsDefs
-- import qualified TxsShow
-- import           TxsUtils

-- import from solve
-- import qualified FreeVar
-- import qualified SMT
-- import qualified Solve
-- import qualified Solve.Params
-- import qualified SolveDefs
-- import qualified SMTData

-- import from value
-- import qualified Eval

-- import from lpe
-- import qualified LPE
-- import qualified LPE

-- import from valexpr
-- import qualified SortId
-- import qualified SortOf
-- import           ConstDefs
-- import           VarId


-- ----------------------------------------------------------------------------------------- --
-- | n-complete test purposes generation.
--
--   Only possible when Initing.
txsNComplete :: TxsDefs.ModelDef                              -- ^ model: currently only
                                                              -- `StautDef` without data
             -> IOC.IOC (Either EnvData.Msg TxsDefs.PurpId)   -- ^ constructed purpose
txsNComplete (TxsDefs.ModelDef insyncs outsyncs splsyncs bexp)  =  do
     envc <- get
     case (IOC.state envc, TxsDefs.view bexp) of
       ( IOC.Initing { IOC.tdefs = tdefs }
       , TxsDefs.ProcInst procid@(TxsDefs.ProcId pnm _ _ _ _) chans []
       ) | and [ Set.size sync == 1 | sync <- insyncs ++ outsyncs ]
           && and [ null srts
                  | TxsDefs.ChanId _ _ srts <- Set.toList $ Set.unions $ insyncs ++ outsyncs
                  ]
           && null splsyncs
         -> case Map.lookup procid (TxsDefs.procDefs tdefs) of
              Just (TxsDefs.ProcDef chids [] staut@(TxsDefs.view -> TxsDefs.StAut _ ve _))
                   | Map.null ve
                -> do let chanmap                       = Map.fromList (zip chids chans)
                           TxsDefs.StAut statid _ trans = TxsDefs.view $ Expand.relabel chanmap staut
                       maypurp <- NComp.nComplete insyncs outsyncs statid trans
                       case maypurp of
                         Just purpdef -> do
                           uid <- gets IOC.unid
                           let purpid = TxsDefs.PurpId ("PURP_"<>pnm) (uid+1)
                               tdefs' = tdefs
                                 { TxsDefs.purpDefs =
                                     Map.insert purpid purpdef (TxsDefs.purpDefs tdefs)
                                 }
                           IOC.incUnid
                           IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs' }
                           return $ Right purpid
                         _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                                              "Could not construct a test prurpose"
              _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                                   "N-Complete requires a data-less STAUTDEF"
       _ -> return $ Left $ EnvData.TXS_CORE_USER_ERROR
                            "N-Complete only in Initing Mode with a StAutDef"

-- ----------------------------------------------------------------------------------------- --
-- | LPE transformation.
--
--   Only possible when Initing.
txsLPE :: Either TxsDefs.BExpr TxsDefs.ModelId   -- ^ either a behaviour expression (that shall
                                                 --   be a process instantiation), or a model
                                                 --   definition (that shall contain a process
                                                 --   instantiation), to be tranformed
       -> IOC.IOC (Either EnvData.Msg (Either TxsDefs.BExpr TxsDefs.ModelId))
                                                 -- ^ transformed process instantiation
                                                 --   or model definition
txsLPE (Left bexpr)  =  do
  envc <- get
  case IOC.state envc of
    IOC.Initing {IOC.tdefs = tdefs}
      -> do lpe <- LPE.lpeTransform bexpr (TxsDefs.procDefs tdefs)
            case lpe of
              Just (procinst'@(TxsDefs.view -> TxsDefs.ProcInst procid' _ _), procdef')
                -> case Map.lookup procid' (TxsDefs.procDefs tdefs) of
                     Nothing
                       -> do let tdefs' = tdefs { TxsDefs.procDefs = Map.insert
                                                    procid' procdef' (TxsDefs.procDefs tdefs)
                                                }
                             IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs' }
                             return $ Just (Left procinst')
                     _ -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                           "LPE: generated process id already exists" ]
                             return Nothing
              _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "LPE: transformation failed" ]
                      return Nothing
    _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "LPE: only allowed if initialized" ]
            return Nothing

txsLPE (Right modelid@(TxsDefs.ModelId modname _moduid))  =  do
  envc <- get
  case IOC.state envc of
    IOC.Initing {IOC.tdefs = tdefs}
      -> case Map.lookup modelid (TxsDefs.modelDefs tdefs) of
           Just (TxsDefs.ModelDef insyncs outsyncs splsyncs bexpr)
             -> do lpe' <- txsLPE (Left bexpr)
                   lift $ hPrint stderr lpe'
                   case lpe' of
                     Just (Left (procinst'@(TxsDefs.view -> TxsDefs.ProcInst{})))
                       -> do uid'   <- IOC.newUnid
                             tdefs' <- gets (IOC.tdefs . IOC.state)
                             let modelid' = TxsDefs.ModelId ("LPE_"<>modname) uid'
                                 modeldef'= TxsDefs.ModelDef insyncs outsyncs splsyncs procinst'
                                 tdefs''  = tdefs'
                                   { TxsDefs.modelDefs = Map.insert modelid' modeldef'
                                                                    (TxsDefs.modelDefs tdefs')
                                   }
                             IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs'' }
                             return $ Just (Right modelid')
                     _ -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "LPE: " ++
                                           "transformation on behaviour of modeldef failed" ]
                             return Nothing
           _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "LPE: model not defined" ]
                   return Nothing
    _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "LPE: only allowed if initialized" ]
            return Nothing


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

