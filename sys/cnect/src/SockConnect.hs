{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- {-# LANGUAGE OverloadedStrings #-}

module SockConnect

-- ----------------------------------------------------------------------------------------- --
--
-- Control, Input, Output for Connections to the Outside World
-- In  = From Outside World
-- Out = To   Outside World
--
-- ----------------------------------------------------------------------------------------- --
-- export

( ToW            -- type ( Chan DD.SAction, ThreadId  , [DD.ConnHandle] )
, FroW           -- type ( Chan DD.SAction, [ThreadId], [DD.ConnHandle] )
, openSockets    -- :: CnectType -> [ConnDef] -> IO (ToW,FroW)
, closeSockets   -- :: ToW -> FroW -> IO ()
, putSocket      -- :: Int -> Int -> ToW -> FroW -> DD.Action -> IOC.IOC DD.Action
, getSocket      -- :: Int -> FroW -> IOC.IOC DD.Action
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.State
import           System.Timeout

import qualified Data.Text           as T

import           Network.TextViaSockets (Connection)
import qualified Network.TextViaSockets as TVS

-- import from local
import           EnDecode

-- import from coreenv
import qualified EnvCore             as IOC

-- import from defs
import qualified TxsDDefs            as DD
import qualified TxsDefs             as D


-- ----------------------------------------------------------------------------------------- --
-- types

type ToW    =  ( Chan DD.SAction, ThreadId  , [DD.ConnHandle] )
type FroW   =  ( Chan DD.SAction, [ThreadId], [DD.ConnHandle] )


-- ----------------------------------------------------------------------------------------- --
-- openSockets :  open connections

openSockets :: D.CnectType -> [D.ConnDef] -> IO (ToW,FroW)
openSockets cnectType connDefs  =  do
     (towHdls, frowHdls) <- case cnectType of
                              D.ClientSocket -> openClientSockets connDefs
                              D.ServerSocket -> openServerSockets connDefs
     towChan     <- newChan
     frowChan    <- newChan
     towThread   <- forkIO $ towChanThread towChan
     frowThreads <- sequence [ forkIO $ frowChanThread c frowChan
                             | DD.ConnHfroW _ c _ _ <- frowHdls
                             ]
     return ( ( towChan , towThread  , towHdls  )
            , ( frowChan, frowThreads, frowHdls )
            )

-- ----------------------------------------------------------------------------------------- --

towChanThread :: Chan DD.SAction -> IO ()
towChanThread towchan  =  do
     sact <- readChan towchan
     case sact of
       DD.SAct c s -> do TVS.putLineTo c s
                         towChanThread towchan
       DD.SActQui  -> towChanThread towchan

-- ----------------------------------------------------------------------------------------- --

frowChanThread :: Connection -> Chan DD.SAction -> IO ()
frowChanThread c frowchan  =  do
     s <- TVS.getLineFrom c
     writeChan frowchan (DD.SAct c s)
     frowChanThread c frowchan

-- ----------------------------------------------------------------------------------------- --

openClientSockets :: [D.ConnDef] -> IO ([DD.ConnHandle],[DD.ConnHandle])
openClientSockets conndefs  =  do
     let tofrosocks = [ (ctow, vars, vexp, cfrow, var, vexps, htow, ptow)
                      | D.ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , D.ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                      , htow == hfrow, ptow == pfrow
                      ]
         tosocks    = [ (ctow, vars, vexp, htow, ptow)
                      | D.ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                      ]
         frosocks   = [ (cfrow, var, vexps, hfrow, pfrow)
                      | D.ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                      , (hfrow,pfrow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                      ]
     tofroConns <- sequence [ TVS.connectTo (T.unpack hst) (show prt)
                            | (_, _, _, _, _, _, hst, prt) <- tofrosocks
                            ]
     toConns    <- sequence [ TVS.connectTo (T.unpack hst) (show prt)
                            | (_, _, _, hst, prt) <- tosocks
                            ]
     froConns   <- sequence [ TVS.connectTo (T.unpack hst) (show prt)
                            | (_, _, _, hst, prt) <- frosocks
                            ]
     let towhdls  = [ DD.ConnHtoW ctow c vars vexp
                    | ( (ctow, vars, vexp, _, _, _, _, _), c ) <- zip tofrosocks tofroConns
                    ] ++
                    [ DD.ConnHtoW ctow c vars vexp
                    | ( (ctow, vars, vexp, _, _), c ) <- zip tosocks toConns
                    ]
         frowhdls = [ DD.ConnHfroW cfrow c var vexps
                    | ( (_, _, _, cfrow, var, vexps, _, _), c ) <- zip tofrosocks tofroConns
                    ]++
                    [ DD.ConnHfroW cfrow c var vexps
                    | ( (cfrow, var, vexps, _, _), c ) <- zip frosocks froConns
                    ]
     return ( towhdls, frowhdls )

-- ----------------------------------------------------------------------------------------- --

openServerSockets :: [D.ConnDef] -> IO ([DD.ConnHandle],[DD.ConnHandle])
openServerSockets conndefs  =  do
     let tofrosocks = [ (ctow, vars, vexp, cfrow, var, vexps, htow, ptow)
                      | D.ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , D.ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                      , htow == hfrow , ptow == pfrow
                      ]
         tosocks    = [ (ctow, vars, vexp, htow, ptow)
                      | D.ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                      ]
         frosocks   = [ (cfrow, var, vexps, hfrow, pfrow)
                      | D.ConnDfroW cfrow hfrow pfrow var vexps  <- conndefs
                      , (hfrow,pfrow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                      ]
     tofroConnsA <- async $ mapConcurrently TVS.acceptOn
                                 [ fromInteger prt
                                 | (_, _, _, _, _, _, _, prt) <- tofrosocks
                                 ]
     toConnsA    <- async $ mapConcurrently TVS.acceptOn
                                 [ fromInteger prt
                                 | (_, _, _, _, prt) <- tosocks
                                 ]
     froConnsA   <- async $ mapConcurrently TVS.acceptOn
                                 [ fromInteger prt
                                 | (_, _, _, _, prt) <- frosocks
                                 ]
     tofroConns  <- wait tofroConnsA
     toConns     <- wait toConnsA
     froConns    <- wait froConnsA
     let towConns  = [ DD.ConnHtoW ctow c vars vexp
                     | ( (ctow, vars, vexp, _, _, _, _, _), c ) <- zip tofrosocks tofroConns
                     ]++
                     [ DD.ConnHtoW ctow c vars vexp
                     | ( (ctow, vars, vexp, _, _), c ) <- zip tosocks toConns
                     ]
         frowConns = [ DD.ConnHfroW cfrow c var vexps
                     | ( (_, _, _, cfrow, var, vexps, _, _), c ) <- zip tofrosocks tofroConns
                     ]++
                     [ DD.ConnHfroW cfrow c var vexps
                     | ( (cfrow, var, vexps, _, _), c ) <- zip frosocks froConns
                     ]
     return ( towConns, frowConns )


-- ----------------------------------------------------------------------------------------- --
-- closeSockets :  close connections

closeSockets :: ToW -> FroW -> IO ()
closeSockets ( _chanToW, towThread, towhdls ) ( _chanFroW, frowThreads, frowhdls )  =  do
     killThread towThread
     mapM_ killThread frowThreads
     mapM_ TVS.close [ c | DD.ConnHtoW  _ c _ _ <- towhdls  ]
     mapM_ TVS.close [ c | DD.ConnHfroW _ c _ _ <- frowhdls ]
     return ()


-- ----------------------------------------------------------------------------------------- --
-- putSocket :  try to do output to world, or observe earlier input (no quiescence)

putSocket :: Int -> Int -> ToW -> FroW -> DD.Action -> IOC.IOC DD.Action

putSocket _dtime chtime (towChan,_,towHdls) (frowChan,_,frowHdls) act@DD.Act{}  =  do
     sact <- EnDecode.encode towHdls act
     obs  <- lift $ timeout (chtime*1000) (readChan frowChan)             -- timeout in musec
     case obs of
       Nothing            -> do lift $ writeChan towChan sact
                                return act
       Just DD.SActQui    -> do lift $ writeChan towChan sact
                                return act
       Just (DD.SAct c s) -> EnDecode.decode frowHdls (DD.SAct c s)

putSocket dtime chtime (towChan,_,towHdls) (frowChan,_,frowHdls) DD.ActQui  =  do
     sact <- EnDecode.encode towHdls DD.ActQui
     obs  <- lift $ timeout (chtime*1000) (readChan frowChan)       -- timeout in musec
     case obs of
       Nothing            -> do lift $ threadDelay (dtime*1000)
                                lift $ writeChan towChan sact
                                return DD.ActQui
       Just DD.SActQui    -> do lift $ threadDelay (dtime*1000)
                                lift $ writeChan towChan sact
                                return DD.ActQui
       Just (DD.SAct c s) -> EnDecode.decode frowHdls (DD.SAct c s)


-- ----------------------------------------------------------------------------------------- --
-- getSocket :  observe input from world, or observe quiescence

getSocket :: Int -> FroW -> IOC.IOC DD.Action
getSocket dtime (frowChan,_,frowHdls)  =  do
     obs <- lift $ timeout (dtime*1000) (readChan frowChan)
     case obs of       
       Nothing            -> return DD.ActQui
       Just DD.SActQui    -> return DD.ActQui
       Just (DD.SAct c s) -> EnDecode.decode frowHdls (DD.SAct c s)


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

