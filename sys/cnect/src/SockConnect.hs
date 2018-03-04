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

( ToW                 -- type ( Chan TxsDDefs.SAction, ThreadId  , [TxsDDefs.ConnHandle] )
, FroW                -- type ( Chan TxsDDefs.SAction, [ThreadId], [TxsDDefs.ConnHandle] )
, openCnectSockets    -- :: CnectType -> [ConnDef] -> IO (ToW,FroW)
, closeCnectSockets   -- :: ToW -> FroW -> IO ()
, putCnectSocket      -- :: Int -> ToW -> FroW -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
, getCnectSocket      -- :: Int -> FroW -> IOC.IOC TxsDDefs.Action
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
-- types

type ToW    =  ( Chan TxsDDefs.SAction, ThreadId  , [TxsDDefs.ConnHandle] )
type FroW   =  ( Chan TxsDDefs.SAction, [ThreadId], [TxsDDefs.ConnHandle] )


-- ----------------------------------------------------------------------------------------- --
-- openCnectSockets :  open connections

openCnectSockets :: CnectType -> [ConnDef] -> IO (ToW,FroW)
openCnectSockets cnectType connDefs  =  do
     (towHdls, frowHdls) <- case cnectType of
                              ClientSocket -> openCnectClientSockets connDefs
                              ServerSocket -> openCnectServerSockets connDefs
     towChan     <- newChan
     frowChan    <- newChan
     towThread   <- forkIO $ towChanThread towChan
     frowThreads <- sequence [ forkIO $ frowChanThread c frowChan
                             | ConnHfroW _ c _ _ <- frowHdls
                             ]
     return ( ( towChan , towThread  , towHdls  )
            , ( frowChan, frowThreads, frowHdls )
            )

-- ----------------------------------------------------------------------------------------- --

towChanThread :: Chan SAction -> IO ()
towChanThread towchan  =  do
     sact <- readChan towchan
     case sact of
       SAct c s -> do TVS.putLineTo c s
                      towChanThread towchan
       SActQui  -> towChanThread towchan

-- ----------------------------------------------------------------------------------------- --

frowChanThread :: Connection -> Chan SAction -> IO ()
frowChanThread c frowchan  =  do
     s <- TVS.getLineFrom c
     writeChan frowchan (SAct c s)
     frowChanThread c frowchan

-- ----------------------------------------------------------------------------------------- --

openCnectClientSockets :: [ConnDef] -> IO ([ConnHandle],[ConnHandle])
openCnectClientSockets conndefs  =  do
     let tofrosocks = [ (ctow, vars, vexp, cfrow, var, vexps, htow, ptow)
                      | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                      , htow == hfrow, ptow == pfrow
                      ]
         tosocks    = [ (ctow, vars, vexp, htow, ptow)
                      | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                      ]
         frosocks   = [ (cfrow, var, vexps, hfrow, pfrow)
                      | ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
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
     let towhdls  = [ ConnHtoW ctow c vars vexp
                    | ( (ctow, vars, vexp, _, _, _, _, _), c ) <- zip tofrosocks tofroConns
                    ] ++
                    [ ConnHtoW ctow c vars vexp
                    | ( (ctow, vars, vexp, _, _), c ) <- zip tosocks toConns
                    ]
         frowhdls = [ ConnHfroW cfrow c var vexps
                    | ( (_, _, _, cfrow, var, vexps, _, _), c ) <- zip tofrosocks tofroConns
                    ]++
                    [ ConnHfroW cfrow c var vexps
                    | ( (cfrow, var, vexps, _, _), c ) <- zip frosocks froConns
                    ]
     return ( towhdls, frowhdls )

-- ----------------------------------------------------------------------------------------- --

openCnectServerSockets :: [ConnDef] -> IO ([ConnHandle],[ConnHandle])
openCnectServerSockets conndefs  =  do
     let tofrosocks = [ (ctow, vars, vexp, cfrow, var, vexps, htow, ptow)
                      | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                      , htow == hfrow , ptow == pfrow
                      ]
         tosocks    = [ (ctow, vars, vexp, htow, ptow)
                      | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                      , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                      ]
         frosocks   = [ (cfrow, var, vexps, hfrow, pfrow)
                      | ConnDfroW cfrow hfrow pfrow var vexps  <- conndefs
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
     let towConns  = [ ConnHtoW ctow c vars vexp
                     | ( (ctow, vars, vexp, _, _, _, _, _), c ) <- zip tofrosocks tofroConns
                     ]++
                     [ ConnHtoW ctow c vars vexp
                     | ( (ctow, vars, vexp, _, _), c ) <- zip tosocks toConns
                     ]
         frowConns = [ ConnHfroW cfrow c var vexps
                     | ( (_, _, _, cfrow, var, vexps, _, _), c ) <- zip tofrosocks tofroConns
                     ]++
                     [ ConnHfroW cfrow c var vexps
                     | ( (cfrow, var, vexps, _, _), c ) <- zip frosocks froConns
                     ]
     return ( towConns, frowConns )


-- ----------------------------------------------------------------------------------------- --
-- closeCnectSockets :  close connections

closeCnectSockets :: ToW -> FroW -> IO ()
closeCnectSockets ( _chan, towThread, towhdls ) ( _chan, frowThreads, frowhdls )  =  do
     killThread towThread
     mapM_ killThread frowThreads
     mapM_ TVS.close [ c | ConnHtoW  _ c _ _ <- towhdls  ]
     mapM_ TVS.close [ c | ConnHfroW _ c _ _ <- frowhdls ]
     return ()


-- ----------------------------------------------------------------------------------------- --
-- putCnectSocket :  try to do output to world, or observe earlier input (no quiescence)

putCnectSocket :: Int -> Int -> ToW -> FroW -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action

putCnectSocket dtime chtime (towChan,_,towHdls) (frowChan,_,frowHdls) act@Act{}  =  do
     sact <- EnDecode.encode towHdls act
     obs  <- lift $ (chReadTime*1000) (readChan frowChan)                 -- timeout in musec
     case obs of
       Nothing         -> do lift $ writeChan towChan sact
                             return act
       Just SActQui    -> do lift $ writeChan towChan sact
                             return act
       Just (SAct c s) -> EnDecode.decode frowHdls (SAct c s)

putCnectSocket :: dtime chtime (towChan,_,towHdls) (frowChan,_,frowHdls) ActQui  =  do
     sact <- EnDecode.encode towHandles ActQui
     obs  <- lift $ (chtime*1000) (readChan frowChan)       -- timeout in musec
     case obs of
       Nothing         -> do lift $ threadDelay (dtime*1000)
                             lift $ writeChan towChan sact
                             return ActQui
       Just SActQui    -> do lift $ threadDelay (dtime*1000)
                             lift $ writeChan towChan sact
                             return ActQui
       Just (SAct c s) -> EnDecode.decode frowHdls (SAct c s)


-- ----------------------------------------------------------------------------------------- --
-- getCnectSocket :  observe input from world, or observe quiescence

getCnectSocket :: Int -> FroW -> IOC.IOC TxsDDefs.Action
getCnectSocket dtime (frowChan,_,frowHdls)  =  do
     obs <- lift $ timeout (deltaTime*1000) (readChan frowChan)
     case obs of       
       Nothing         -> return ActQui
       Just SActQui    -> return ActQui
       Just (SAct c s) -> EnDecode.decode frowHdls (SAct c s)


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

