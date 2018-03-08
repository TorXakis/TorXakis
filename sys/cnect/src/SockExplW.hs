{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- {-# LANGUAGE OverloadedStrings #-}

module SockExplW

-- ----------------------------------------------------------------------------------------- --
--
-- Control, Input, Output for Connections to the Outside World
-- In  = From Outside World
-- Out = To   Outside World
--
-- ----------------------------------------------------------------------------------------- --
-- export

( SockExplW (..)
, setSockExplW   -- CnectDef -> Int -> Int -> Int -> SockExplW
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State
import           System.IO
import           System.Process
import           GHC.Conc
import           Data.Maybe
import qualified Data.Char          as Char
import qualified Data.Text          as T

-- import from local
import           SockConnect

-- import from coreenv
import qualified EnvCore            as IOC

-- import from behave
import qualified EnvData

-- import from defs
import qualified TxsDDefs           as DD
import qualified TxsDefs            as D


-- ----------------------------------------------------------------------------------------- --
-- socket explicit world as eworld

data SockExplW  =  IdleSockExplW { cnectdef   :: D.CnectDef
                                 , conndelay  :: Int  -- ^ msec delay between start and connect
                                 , deltatime  :: Int  -- ^ quiescence timer
                                 , chreadtime :: Int  -- ^ channel delay before reading
                                 }
                 | RunSockExplW  { cnectdef   :: D.CnectDef
                                 , conndelay  :: Int
                                 , deltatime  :: Int
                                 , chreadtime :: Int
                                 , tow        :: ToW      -- ^ connections to external world
                                 , frow       :: FroW     -- ^ connections from external world
                                 , ewph       :: ProcessHandle  -- ^ external world process
                                 }

instance IOC.EWorld SockExplW
  where
     startW     =  startSockExplW
     stopW      =  stopSockExplW
     putToW     =  putSockExplW
     getFroW    =  getSockExplW
     chansToW   =  chansSockToW
     chansFroW  =  chansSockFroW


chansSockToW :: SockExplW -> [D.ChanId]
chansSockToW sew
  =  case sew of 
       IdleSockExplW { cnectdef = D.CnectSockExplW _cmdw _ctype conndefs }
         -> [ chan | D.ConnDtoW  chan _ _ _ _ <- conndefs ]
       RunSockExplW  { cnectdef = D.CnectSockExplW _cmdw _ctype conndefs }
         -> [ chan | D.ConnDtoW  chan _ _ _ _ <- conndefs ]
       _ -> error "SockExplW function called with CnectSockImplW: should not have happened\n"

chansSockFroW :: SockExplW -> [D.ChanId]
chansSockFroW sew
  =  case sew of 
       IdleSockExplW { cnectdef = D.CnectSockExplW _cmdw _ctype conndefs }
         -> [ chan | D.ConnDfroW chan _ _ _ _ <- conndefs ]
       RunSockExplW  { cnectdef = D.CnectSockExplW _cmdw _ctype conndefs }
         -> [ chan | D.ConnDfroW chan _ _ _ _ <- conndefs ]
       _ -> error "SockExplW function called with CnectSockImplW: should not have happened\n"


-- ----------------------------------------------------------------------------------------- --
-- setSockWorld :  set and define, without starting socket world

setSockExplW :: D.CnectDef -> Int -> Int -> Int -> SockExplW
setSockExplW cnectDef connDelay deltaTime chReadTime
  =  IdleSockExplW cnectDef connDelay deltaTime chReadTime


-- ----------------------------------------------------------------------------------------- --
-- startSockWorld :  start socket world if Idle, otherwise do nothing

startSockExplW :: SockExplW -> IOC.IOC SockExplW
startSockExplW sew  =
     case sew of
       IdleSockExplW cnectDef@(D.CnectSockExplW cmdw ctype cdefs)
         connDelay deltaTime chReadTime
         | not $ and $ map Char.isSpace $ T.unpack cmdw
         -> let cmd = words $ T.unpack cmdw
             in do (Just hin, Just hout, Just herr, procH) <- lift $ createProcess
                      ( proc (head cmd) (tail cmd) )
                      { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }
                   lift $ hSetBuffering hin  NoBuffering
                   lift $ hSetBuffering hout NoBuffering
                   lift $ hSetBuffering herr NoBuffering
                   lift $ threadDelay (1000*connDelay)
                   (toW,froW) <- lift $ openSockets ctype cdefs
                   return $ RunSockExplW cnectDef connDelay deltaTime chReadTime toW froW procH
       _ -> return sew


-- ----------------------------------------------------------------------------------------- --
-- stopSockWorld :  stop socket world if Running, otherwise do nothing

stopSockExplW :: SockExplW -> IOC.IOC SockExplW
stopSockExplW sew  =
     case sew of
       RunSockExplW cnectDef connDelay deltaTime chReadTime toW froW ewPh
         -> do ec <- lift $ getProcessExitCode ewPh
               if  isNothing ec
                 then lift $ terminateProcess ewPh
                 else return ()
               lift $ closeSockets toW froW
               return $ IdleSockExplW cnectDef connDelay deltaTime chReadTime
       _ -> return sew

 
-- ----------------------------------------------------------------------------------------- --
-- putSockWorld :  try to do output to world, or observe earlier input (no quiescence)

putSockExplW :: SockExplW -> DD.Action -> IOC.IOC DD.Action

putSockExplW sew act@DD.Act{}  =
     case sew of
       RunSockExplW _cdef _cdelay dtime chtime tw frw _ewph
         -> putSocket dtime chtime tw frw act
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Sending action to world while no world running" ]
               return act

putSockExplW sew DD.ActQui  =
     case sew of
       RunSockExplW _cdef _cdelay deltaTime chReadTime tw frw _ewph
         -> putSocket deltaTime chReadTime tw frw DD.ActQui
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Sending action to world while no world running" ]
               return DD.ActQui


-- ----------------------------------------------------------------------------------------- --
-- getSockWorld :  observe input from world, or observe quiescence

getSockExplW :: SockExplW -> IOC.IOC DD.Action
getSockExplW sew  =
     case sew of
       RunSockExplW _cnectdef _cdelay dtime _chtime _tw frw _ewph
         -> getSocket dtime frw
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Observing action from world while no world running" ]
               return DD.ActQui


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

