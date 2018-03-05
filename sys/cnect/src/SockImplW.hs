{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- {-# LANGUAGE OverloadedStrings #-}

module SockImplW

-- ----------------------------------------------------------------------------------------- --
--
-- Control, Input, Output for Connections to the Outside World
-- In  = From Outside World
-- Out = To   Outside World
--
-- ----------------------------------------------------------------------------------------- --
-- export

( SockImplW (..)
, setSockImplW   -- CnectDef -> Int -> Int -> Int -> SockImplW
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import           Control.Concurrent
-- import           Control.Concurrent.Async
import           Control.Monad.State
-- import           System.IO
-- import           System.Process
-- import GHC.Conc
-- import qualified Data.Char           as Char


-- import qualified Data.Text           as T
-- import           System.Timeout
-- import qualified Data.Map            as Map

-- import           Network.TextViaSockets (Connection)
-- import qualified Network.TextViaSockets as TVS

-- import from local
import           SockConnect

import qualified EnvData

-- import from coreenv
import qualified EnvCore             as IOC

-- import from defs
import qualified TxsDDefs            as DD
import qualified TxsDefs             as D


-- ----------------------------------------------------------------------------------------- --
-- socketworld as eworld

data SockImplW  =  IdleSockImplW { cnectdef   :: D.CnectDef
                                 , conndelay  :: Int -- ^ msec delay between start and connect
                                 , deltatime  :: Int -- ^ quiescence timer
                                 , chreadtime :: Int -- ^ channel delay before reading
                                 }
                 | RunSockImplW  { cnectdef   :: D.CnectDef
                                 , conndelay  :: Int
                                 , deltatime  :: Int
                                 , chreadtime :: Int
                                 , tow        :: ToW      -- ^ connections to external world
                                 , frow       :: FroW     -- ^ connections from external world
                                 }

instance IOC.EWorld SockImplW
  where
     startW    =  startSockImplW
     stopW     =  stopSockImplW
     putToW    =  putSockImplW
     getFroW   =  getSockImplW


-- ----------------------------------------------------------------------------------------- --
-- setSockWorld :  set and define, without starting socket world

setSockImplW :: D.CnectDef -> Int -> Int -> Int -> SockImplW
setSockImplW cnectDef connDelay deltaTime chReadTime
  =  IdleSockImplW cnectDef connDelay deltaTime chReadTime


-- ----------------------------------------------------------------------------------------- --
-- startSockWorld :  start socket world if Idle, otherwise do nothing

startSockImplW :: SockImplW -> IOC.IOC SockImplW
startSockImplW siw  =
     case siw of
       IdleSockImplW cnectDef@(D.CnectSockImplW ctype cdefs) connDelay deltaTime chReadTime
         -> do (toW,froW) <- lift $ openSockets ctype cdefs
               return $ RunSockImplW cnectDef connDelay deltaTime chReadTime toW froW
       _ -> return siw


-- ----------------------------------------------------------------------------------------- --
-- stopSockWorld :  stop socket world if Running, otherwise do nothing

stopSockImplW :: SockImplW -> IOC.IOC SockImplW
stopSockImplW siw  =
     case siw of
       RunSockImplW cnectDef connDelay deltaTime chReadTime toW froW
         -> do lift $ closeSockets toW froW
               return $ IdleSockImplW cnectDef connDelay deltaTime chReadTime
       _ -> return siw
 

-- ----------------------------------------------------------------------------------------- --
-- putSockWorld :  try to do output to world, or observe earlier input (no quiescence)

putSockImplW :: SockImplW -> DD.Action -> IOC.IOC DD.Action

putSockImplW siw act@DD.Act{}  =
     case siw of
       RunSockImplW _cdef _cdelay dtime chtime tw frw
         -> putSocket dtime chtime tw frw act
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Sending action to world while no world running" ]
               return act

putSockImplW siw DD.ActQui  =
     case siw of
       RunSockImplW _cdef _cdelay dtime chtime tw frw
         -> putSocket dtime chtime tw frw DD.ActQui
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Sending action to world while no world running" ]
               return DD.ActQui


-- ----------------------------------------------------------------------------------------- --
-- getSockWorld :  observe input from world, or observe quiescence

getSockImplW :: SockImplW -> IOC.IOC DD.Action
getSockImplW siw  =
     case siw of
       RunSockImplW _cnectdef _cdelay dtime _chtime _tw frw
         -> getSocket dtime frw
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Observing action from world while no world running" ]
               return DD.ActQui


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

