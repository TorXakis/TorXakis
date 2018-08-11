{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-----------------------------------------------------------------------------
-- |
-- Module      :  TxsCoreShow
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- API for TorXakis core functionality.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}


module TxsCoreShow
(
  -- * show item
  txsShow

)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           System.IO
import           System.Random

-- import from local
import           CoreUtils
import           Ioco
import           Mapper
import           NComp
import           Purpose

import           Config              (Config)
import qualified Config

-- import from bexpr
import           Relabel             (relabel)

-- import from behave(defs)
import qualified Behave
import qualified BTree

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData
import qualified ParamCore

-- import from defs
import qualified Sigs
import qualified TxsDDefs
import qualified TxsDefs
import qualified TxsShow
import           TxsUtils

-- import from solve
import qualified FreeVar
import qualified SMT
import qualified Solve
import qualified Solve.Params
import qualified SolveDefs
import qualified SMTData

-- import from value
import qualified Eval

-- import from lpe
-- import qualified LPE
import qualified LPE

-- import from valexpr
import qualified SortId
import qualified SortOf
import Constant
import VarId


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --


-- | Show provided item.
txsShow :: String               -- ^ kind of item to be shown.
        -> String               -- ^ name of item to be shown.
                                --   Valid items are "tdefs", "state",
                                --   "model", "mapper", "purp", "modeldef" \<name>,
                                --   "mapperdef" \<name>, "purpdef" \<name>
        -> IOC.IOC String
txsShow item nm  = do
     envc  <- gets IOC.state
     let tdefs = IOC.tdefs envc
     case envc of
      IOC.Noning{ }
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Noning: nothing to be shown" ]
               return "\n"
      IOC.Initing{ }
         -> case (item,nm) of
              ("tdefs"    ,"") -> return $ show (IOC.tdefs envc)
              ("modeldef" ,_) -> return $ nm2string nm TxsDefs.IdModel TxsDefs.DefModel
                                                     (TxsDefs.modelDefs tdefs)
              ("mapperdef",_) -> return $ nm2string nm TxsDefs.IdMapper TxsDefs.DefMapper
                                                     (TxsDefs.mapperDefs tdefs)
              ("purpdef"  ,_) -> return $ nm2string nm TxsDefs.IdPurp TxsDefs.DefPurp
                                                     (TxsDefs.purpDefs tdefs)
              ("procdef"  ,_) -> return $ nm2string nm TxsDefs.IdProc TxsDefs.DefProc
                                                     (TxsDefs.procDefs tdefs)
              ("funcdef"  ,_) -> return $ nm2string nm TxsDefs.IdFunc TxsDefs.DefFunc
                                                     (TxsDefs.funcDefs tdefs)
              _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "nothing to be shown 1" ]
                      return "\n"
      _ -> case (item,nm) of
              ("tdefs"    ,"") -> return $ show (IOC.tdefs envc)
              ("state"    ,"") -> return $ show (IOC.curstate envc)
              ("model"    ,"") -> return $ TxsShow.fshow (IOC.modsts envc)
              ("mapper"   ,"") -> return $ TxsShow.fshow (IOC.mapsts envc)
              ("purp"     ,"") -> return $ TxsShow.fshow (IOC.purpsts envc)
              ("modeldef" ,_)  -> return $ nm2string nm TxsDefs.IdModel TxsDefs.DefModel
                                                     (TxsDefs.modelDefs tdefs)
              ("mapperdef",_)  -> return $ nm2string nm TxsDefs.IdMapper TxsDefs.DefMapper
                                                     (TxsDefs.mapperDefs tdefs)
              ("purpdef"  ,_)  -> return $ nm2string nm TxsDefs.IdPurp TxsDefs.DefPurp
                                                     (TxsDefs.purpDefs tdefs)
              ("procdef"  ,_)  -> return $ nm2string nm TxsDefs.IdProc TxsDefs.DefProc
                                                     (TxsDefs.procDefs tdefs)
              ("funcdef"  ,_) -> return $ nm2string nm TxsDefs.IdFunc TxsDefs.DefFunc
                                                     (TxsDefs.funcDefs tdefs)
              _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "nothing to be shown 2" ]
                      return "\n"

  where
     nm2string :: String
               -> (id -> TxsDefs.Ident)
               -> (def -> TxsDefs.TxsDef)
               -> Map.Map id def
               -> String
     nm2string nm' id2ident id2def iddefs =
       let defs = [ (id2ident id', id2def def) | (id', def) <- Map.toList iddefs
                                              , TxsDefs.name (id2ident id') == T.pack nm' ]
       in case defs of
            [(ident,txsdef)] -> TxsShow.fshow (ident,txsdef)
            _                -> "no (uniquely) defined item to be shown: " ++ nm' ++ "\n"


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

