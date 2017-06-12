{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Sim

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Simulation of a Model
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( simInit   -- :: IOC.IOC ()
, simN      -- :: simN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.Random
import Control.Monad.State

import Ioco
import Mapper
import CoreUtils

import qualified ParamCore   as ParamCore
import qualified EnvCore     as IOC
import qualified EnvData     as EnvData

import qualified TxsDDefs    as TxsDDefs
import qualified TxsShow     as TxsShow
import qualified PShow       as PShow


-- ----------------------------------------------------------------------------------------- --
-- simInit :  initialize models for Simulator


simInit :: IOC.IOC ()
simInit  =  do
     iocoModelInit
     mapperInit
     trieStateInit


-- ----------------------------------------------------------------------------------------- --
-- simN depth :  simulation of depth>0 steps, or infinitely if depth<0


simN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simN depth step  =  do
     [(parname,parval)] <- IOC.getParams ["param_InputCompletion"]
     case read parval of
     { ParamCore.ANGELIC  -> do simA depth step
--   ; ParamCore.DEMONIC  -> do simD depth step
--   ; ParamCore.BUFFERED -> do simB depth step
--   ; _                  -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
--                                            $ "Simulation with unknown input completion" ]
--                              return $ TxsDDefs.Fail TxsDDefs.ActQui
     }


-- ----------------------------------------------------------------------------------------- --
-- simA depth :  angelic simulation of depth>0 steps, or infinitely if depth<0


simA :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simA depth step  =  do
     if  depth == 0
       then do return $ TxsDDefs.Pass
       else do iochoice <- lift $ randomRIO (True, False)
               if  iochoice
                 then do simAfroW depth step
                 else do simAtoW  depth step


simAfroW :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simAfroW depth step  =  do
     getFroW <- gets IOC.getfrow
     act     <- getFroW                                      -- get next output or quiescence
     mact    <- mapperMap act                                -- apply mapper
     case mact of
     { TxsDDefs.Act acts -> do                               -- world provided input to system
          IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                        $ (TxsShow.showN step 6) ++ ":  IN:  "++ (TxsShow.fshow mact) ]
          done <- iocoModelAfter mact                        -- do input in model
          if done
            then do trieStateNext mact
                    simA (depth-1) (step+1)                  -- continue whether done or not
            else do simA (depth-1) (step+1)                  -- continue whether done or not
     ; TxsDDefs.ActQui -> do                                 -- world did not provide input
          simAtoW depth step                                 -- continue with output to world
     }


simAtoW :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simAtoW depth step  =  do
     putToW  <- gets IOC.puttow
     mayAct  <- ranMenuOut
     case mayAct of
     { Just act -> do                                        -- proposed real output or qui
          mact @(TxsDDefs.Act macts ) <- mapperMap act
          mact'@(TxsDDefs.Act macts') <- putToW mact         -- do input on sut, always
          if mact == mact'
            then do IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                                  $ (TxsShow.showN step 6) ++ ": OUT: " ++ (TxsShow.fshow mact) ]
                    done <- iocoModelAfter mact              -- do output in model
                    if done
                      then do trieStateNext mact
                              simA (depth-1) (step+1)        -- continue
                      else do simA (depth-1) (step+1)
            else do case mact' of                            -- input from world was faster
                    { TxsDDefs.Act acts -> do                -- world provided input to system
                         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                                       $ (TxsShow.showN step 6) ++ ":  IN:  "++ (TxsShow.fshow mact) ]
                         done <- iocoModelAfter mact         -- do input in model
                         if done
                           then do trieStateNext mact
                                   simA (depth-1) (step+1)        -- continue
                           else do simA (depth-1) (step+1)
                    ; TxsDDefs.ActQui -> do                  -- world did not provide input
                         simAtoW depth step                  -- continue with output to world
                    }
     ; Nothing -> do                                         -- no proposed output
          IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                        $ "no proposed output: should not happen" ]
          return $ TxsDDefs.NoVerdict
     }


-- ----------------------------------------------------------------------------------------- --
-- randMenuOut :  random output action from menuOut


ranMenuOut :: IOC.IOC (Maybe TxsDDefs.Action)
ranMenuOut  =  do
     menuOut <- iocoModelMenuOut
     isQui   <- iocoModelIsQui
     if isQui
       then do r <- lift $ randomRIO (0, length menuOut)
               if r == 0
                 then return $ Just TxsDDefs.ActQui
                 else randMenu menuOut
       else randMenu menuOut


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

