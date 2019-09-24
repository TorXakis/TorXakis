{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-----------------------------------------------------------------------------
-- |
-- Module      :  TxsCore
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- API for TorXakis core functionality.
-----------------------------------------------------------------------------
module TorXakis.TxsCore
( -- * run TorXakis core
  runTxsCore

  -- * initialize TorXakis core
, txsInit

  -- * terminate TorXakis core
, txsTermit

  -- * evaluation of value expression
, txsEval

)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State

import qualified TorXakis.CoreState             as IOC

import TorXakis.Value
import TorXakis.ValExpr
import TorXakis.ContextValExpr


-- | TorXakis core main api -- start
runTxsCore :: StateT s IOC.IOC a -> s -> IO ()
runTxsCore ctrl s0  =  do
      _ <- runStateT (runTxsCtrl ctrl s0)
              IOC.EnvC { IOC.state  = initState
                       }
      return ()
      where initState = IOC.Noning

runTxsCtrl :: StateT s IOC.IOC a -> s -> IOC.IOC ()
runTxsCtrl ctrl s0  =  do
     _ <- runStateT ctrl s0
     return ()

-- | TorXakis core main api -- modus transition general
txsInit :: ContextValExpr  -- ValExprContext c =>          -- ^ Definitions for computations.
                           -- c
        -> IOC.IOC ()
txsInit tdefs  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> put envc { IOC.state = IOC.Initing { IOC.tdefs = tdefs
                                               }
                     }
       _ -> do txsTermit
               txsInit tdefs 

-- | terminate TorXakis core
txsTermit :: IOC.IOC ()
txsTermit  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> return ()
       _ -> put envc { IOC.state = IOC.Noning }


-- | evaluation of 'TorXakis.ValExpression'
--   Only possible when txscore is initialized.
txsEval :: ValExpression                               -- ^ value expression to be evaluated.
        -> IOC.IOC (Either Error Value)
txsEval vexp  =
     return $ eval vexp


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

