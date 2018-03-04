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

( setSockExplW   -- CnectDef -> Int -> Int -> Int -> SockExplW
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import           Control.Concurrent
-- import           Control.Concurrent.Async
-- import           Control.Monad.State
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
-- import           EnDecode

-- import from serverenv
-- import qualified EnvServer           as IOS
-- import qualified IfServer

-- import from coreenv
-- import qualified EnvCore             as IOC

-- import from defs
-- import           TxsDDefs
-- import           TxsDefs
-- import qualified Utils


-- ----------------------------------------------------------------------------------------- --
-- socket explicit world as eworld

data SockExplW  =  NoneSockExplW
                 | IdleSockExplW { cnectdef   :: TxsDefs.CnectDef
                                 , conndelay  :: Int -- ^ msec delay between start and connect
                                 , deltatime  :: Int -- ^ quiescence timer
                                 , chreadtime :: Int -- ^ channel delay before reading
                                 }
                 | RunSockExplW  { cnectdef   :: TxsDefs.CnectDef
                                 , conndelay  :: Int
                                 , deltatime  :: Int
                                 , chreadtime :: Int
                                 , tow        :: ToW      -- ^ connections to external world
                                 , frow       :: FroW     -- ^ connections from external world
                                 , ewph       :: ProcessHandle -- ^ external world process
                                 }

instance IOC.EWorld SockExplW
  where
     startW    =  startSockExplW
     stopW     =  stopSockExplW
     putToW    =  putSockExplW
     getFroW   =  getSockExplW


-- ----------------------------------------------------------------------------------------- --
-- setSockWorld :  set and define, without starting socket world

setSockExplW :: CnectDef -> Int -> Int -> Int -> SockExplW
setSockExplW cnectdef connDelay deltaTime chReadTime
  =  IdleSockExplW cnectDef connDelay deltaTime chReadTime


-- ----------------------------------------------------------------------------------------- --
-- startSockWorld :  start socket world if Idle, otherwise do nothing

startSockExplW :: SockExplW -> IOC.IOC SockExplW
startSockExplW sew  =
     case sew of
       IdleSockExplW cnectDef@(CnectSockExplW cmdw ctype cdefs) connDelay deltaTime chReadTime
                     | not $ and $ map Char.isSpace $ T.unpack cmdw
         -> let cmd = words $ T.unpack cmdw
             in do (Just hin, Just hout, Just herr, procH) <- lift $ createProcess
                      ( proc (head cmdw) (tail cmdw) )
                      { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }
                   lift $ hSetBuffering hin  NoBuffering
                   lift $ hSetBuffering hout NoBuffering
                   lift $ hSetBuffering herr NoBuffering
                   lift $ threadDelay (1000*connDelay)
                   (toW,froW) <- lift $ openCnectSockets ctype cdefs
                   return $ RunSockExplW cnectDef connDelay deltaTime chReadTime toW froW procH
       _ -> return sew


-- ----------------------------------------------------------------------------------------- --
-- stopSockWorld :  stop socket world if Running, otherwise do nothing

stopSockExplW :: SockExplW -> IOC.IOC SockExplW
stopSockexplW sew  =
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

putSockExplW :: SockExplW -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action

putSockExplW sew act@Act{}  =
     case sew of
       RunSockExplW _cdef _cdelay _dtime chtime tow frow _ewph
         -> putCnectSocket chtime tow frow act
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Sending action to world while no world running" ]
               return act

putSockExplW sew ActQui  =
     case sew of
       RunSockWorld _cdef deltaTime chReadTime tow frow _ewph
         -> putCnectSocket chtime tow frow ActQui
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Sending action to world while no world running" ]
               return ActQui


-- ----------------------------------------------------------------------------------------- --
-- getSockWorld :  observe input from world, or observe quiescence

getSockExplW :: SockExplWorld -> IOC.IOC TxsDDefs.Action
getSockExplW sew  =
     case sew of
       RunSockExplW _cnectdef _cdelay dtime _chtime _tow frow _ewph
         -> getCnectSocket dtime frow
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             "Observing action from world while no world running" ]
               return ActQui


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

