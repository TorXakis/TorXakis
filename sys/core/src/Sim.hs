{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE RecordWildCards #-}

-- ----------------------------------------------------------------------------------------- --

module Sim

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Simulation of a Model
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( simN      -- :: simN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.Random
import Control.Monad.State

import Ioco
import Mapper
import CoreUtils

import qualified ParamCore
import qualified EnvCore     as IOC
import qualified EnvData

import qualified TxsDDefs
import qualified TxsShow


-- ----------------------------------------------------------------------------------------- --
-- simN depth :  simulation of depth>0 steps, or infinitely if depth<0


simN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simN depth step = do
     envc <- get
     [(_,parval)] <- IOC.getParams ["param_InputCompletion"]
     case (read parval, IOC.state envc) of
        { ( ParamCore.ANGELIC
          , IOC.Simuling {}
          ) -> simA depth step
--      ;  ParamCore.DEMONIC  -> simD depth step
--      ;  ParamCore.BUFFERED -> simB depth step
        ; ( _
          , _
          ) -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Incorrect start of simulation" ]
            return TxsDDefs.NoVerdict
        }

-- ----------------------------------------------------------------------------------------- --
-- simA depth :  angelic simulation of depth>0 steps, or infinitely if depth<0

simA :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simA depth step =
     if  depth == 0
       then return TxsDDefs.Pass
       else do iochoice <- lift $ randomRIO (True, False)
               if  iochoice
                 then simAfroW depth step
                 else simAtoW  depth step

simAfroW :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simAfroW depth step = do
     getFroW <- gets (IOC.getfrow . IOC.state)
     act     <- getFroW                                      -- get next output or quiescence
     mact    <- mapperMap act                                -- apply mapper
     case mact of
       TxsDDefs.Act {} -> do                               -- world provided input to system
          IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                        $ TxsShow.showN step 6 ++ ":  IN:  "++ TxsShow.fshow mact ]
          _done <- iocoModelAfter mact                        -- do input in model
          nextBehTrie mact
          simA (depth-1) (step+1)                  -- continue whether _done or not (angelic)
       TxsDDefs.ActQui ->                                    -- world did not provide input
          simAtoW depth step                                 -- continue with output to world

simAtoW :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
simAtoW depth step = do
     putToW  <- gets (IOC.puttow . IOC.state)
     mayAct  <- ranMenuOut
     case mayAct of
       Just act -> do                                        -- proposed real output or qui
         mact  <- mapperMap act                              -- apply mapper
         mact' <- putToW mact                                -- do output to world
         if mact == mact'
           then do
             IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO        -- output to world was done
                           $ TxsShow.showN step 6 ++ ": OUT: " ++ TxsShow.fshow act ]
             done <- iocoModelAfter act                      -- do output in model
             nextBehTrie act
             if  done
               then simA (depth-1) (step+1)               -- continue
               else do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "proposed output could not happen in model" ]
                       return TxsDDefs.NoVerdict
           else do                                           -- input from world was faster
             act' <- mapperMap mact'                          -- map input to model action
             case act' of
               TxsDDefs.Act{} -> do                       -- world provided input to system
                 IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                               $ TxsShow.showN step 6 ++ ":  IN:  "++ TxsShow.fshow act' ]
                 _done <- iocoModelAfter act'                  -- do input in model
                 nextBehTrie act'
                 simA (depth-1) (step+1)           -- continue whether _done or not  (angelic)
               TxsDDefs.ActQui ->                            -- world did not provide input
                 simAtoW depth step                          -- continue with output to world
       Nothing -> do                                         -- no proposed output
          IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "no proposed output: should not happen" ]
          return TxsDDefs.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- randMenuOut :  random output action from menuOut

ranMenuOut :: IOC.IOC (Maybe TxsDDefs.Action)
ranMenuOut = do
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
