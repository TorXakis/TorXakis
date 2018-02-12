{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module SocketWorld

-- ----------------------------------------------------------------------------------------- --
--
-- Control, Input, Output for Connections to the Outside World
-- In  = From Outside World
-- Out = To   Outside World
--
-- ----------------------------------------------------------------------------------------- --
-- export

( initSockWorld      -- :: CnectDef -> Int -> Int -> IOC.IOC IOS.SockWorld
, termitSockWorld    -- :: IOS.SockWorld -> IOC.IOC ()
, SockWorld (..)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.State
-- import           System.IO
-- import           System.Process
-- import GHC.Conc
-- import qualified Data.Char           as Char


import qualified Data.Text           as T
import           System.Timeout
import qualified Data.Map            as Map

import           Network.TextViaSockets (Connection)
import qualified Network.TextViaSockets as TVS

-- import from local
import           EnDecode

-- import from serverenv
-- import qualified EnvServer           as IOS
-- import qualified IfServer

-- import from coreenv
import qualified EnvCore             as IOC

-- import from defs
import           TxsDDefs
import           TxsDefs
-- import qualified Utils


-- ----------------------------------------------------------------------------------------- --
-- socketworld as eworld

type FroW      = ( Maybe (Chan TxsDDefs.SAction), [ThreadId], [TxsDDefs.ConnHandle] )

-- noToW   :: ToW
-- noFroW  :: FroW
-- noToW   =  ( Nothing, Nothing, [] )
-- noFroW  =  ( Nothing, [],      [] )

instance IOC.EWorld SockWorld
  where
     startW    =  startSockWorld
     restartW  =  restartSockWorld
     stopW     =  stopSockWorld
     putToW    =  putSockWorld
     getFroW   =  getSockWorld

data SockWorld =   IdleSW
                 | InitSW   { cnectdef  :: TxsDefs.CnectDef
                            , deltatime :: Int
                            , iotime    :: Int
                            }
                 | CtRunSW  { cnectdef  :: TxsDefs.CnectDef
                            , deltatime :: Int
                            , iotime    :: Int
                            , tow       :: ToW             -- ^ connections to external world
                            , frow      :: FroW            -- ^ connections from external world
                            , proch     :: ProcessHandle
                            }
                 | UCtRunSW { cnectdef  :: TxsDefs.CnectDef
                            , deltatime :: Int
                            , iotime    :: Int
                            , tow       :: ToW              -- ^ connections to external world
                            , frow      :: FroW             -- ^ connections from external world
                             }

-- ----------------------------------------------------------------------------------------- --
-- initSockWorld :  initialize socket world

initSockWorld :: CnectDef -> Int -> Int -> IOC.IOC SockWorld
initSockWorld cnectDef deltaTime ioTime  =
     return $ InitSW cnectDef deltaTime ioTime

-- ----------------------------------------------------------------------------------------- --
-- termitSockWorld :  terminate socket world

termitSockWorld :: SockWorld -> IOC.IOC ()
termitSockWorld _ = return ()

-- ----------------------------------------------------------------------------------------- --
-- startSockWorld :  start socket world, if not already started, otherwise do nothing

startSockWorld :: SockWorld -> IOC.IOC SockWorld
startSockWorld sockWorld  =
 85 >      case sockWorld of
 86 >        InitSW cnectDef@(CnectDef (Just ewCmd) cnectType connDefs) deltaTime ioTime
 87 >             | (not $ and $ map Char.isSpace $ T.unpack ewCmd)
 88 >          -> let cmdw = words $ T.unpack ewCmd
 89 >              in do (Just hin, Just hout, Just herr, procH) <- lift $ createProcess
 90 >                            ( proc (head cmdw) (tail cmdw) )
 91 >                            { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }
 92 >                    lift $ hSetBuffering hin  NoBuffering
 93 >                    lift $ hSetBuffering hout NoBuffering
 94 >                    lift $ hSetBuffering herr NoBuffering
 95 >                    (toW,froW) <- openSockets cnectType connDefs
 96 >                    return $ CtRunSW cnectDef deltaTime ioTime toW froW procH
 97 >        InitSW cnectDef@(CnectDef Nothing cnectType connDefs) deltaTime ioTime
 98 >          -> do (toW,froW) <- openSockets cnectType connDefs
 99 >                return $ UCtRunSW cnectDef deltaTime ioTime toW froW
100 >        _ -> return sockWorld
101 >
102 > -- ----------------------------------------------------------------------------------------- -    -
103 > -- restartSockWorld :  restart socket world, also if already started
104 >
104 >
105 > restartSockWorld :: SockWorld -> IOC.IOC SockWorld
106 > restartSockWorld sockWorld  =
107 >      case sockWorld of
108 >        IdleSW
109 >          -> return $ IdleSW
110 >        InitSW cnectDef@(CnectDef (Just ewCmd) cnectType connDefs) deltaTime ioTime
111 >             | (not $ and $ map Char.isSpace $ T.unpack ewCmd)
112 >          -> let cmdw = words $ T.unpack ewCmd
113 >              in do (Just hin, Just hout, Just herr, procH) <- lift $ createProcess
114 >                            ( proc (head cmdw) (tail cmdw) )
115 >                            { std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }
116 >                    lift $ hSetBuffering hin  NoBuffering
117 >                    lift $ hSetBuffering hout NoBuffering
118 >                    lift $ hSetBuffering herr NoBuffering
119 >                    (toW,froW) <- openSockets cnectType connDefs
120 >                    return $ CtRunSW cnectDef deltaTime ioTime toW froW procH
121 >        InitSW cnectDef@(CnectDef Nothing cnectType connDefs) deltaTime ioTime
122 >          -> do (toW,froW) <- openSockets cnectType connDefs
123 >                return $ UCtRunSW cnectDef deltaTime ioTime toW froW
124 >        _ -> return sockWorld                                              -- already running -    -
125 >
126 > -- ----------------------------------------------------------------------------------------- -    -
127 > -- stopSockWorld :  stop socket world
128 >
128 >
129 > stopSockWorld :: SockWorld -> IOC.IOC SockWorld
130 >
131 > stopSockWorld sockWorld  =
132 >      case sockWorld of
133 >        IdleSW
134 >          -> return $ IdleSW
135 >        InitSW cnectDef deltaTime ioTime
136 >          -> return $ InitSW cnectDef deltaTime ioTime
137 >        CtRunSW cnectDef deltaTime ioTime toW froW procH
138 >          -> do ec <- lift $ getProcessExitCode procH
139 >                if isNothing ec
140 >                  then lift $ terminateProcess procH
141 >                  else return ()
142 >                closeSockets toW froW
143 >                return $ InitSW cnectDef deltaTime ioTime
144 >        UCtRunSW cnectDef deltaTime ioTime toW froW
145 >          -> do closeSockets toW froW
146 >                return $ InitSW cnectDef deltaTime ioTime
147 >
148 > -- ----------------------------------------------------------------------------------------- -    -
149 > -- openSockets ::  open connections
150 >









-- ----------------------------------------------------------------------------------------- --

openSockets :: IOS.IOS ()
openSockets  =  do
     txsmodus <- gets IOS.modus
     cnectdef <- case txsmodus of
                 { IOS.Tested  cdef -> return $ Just cdef
                 ; IOS.Simuled cdef -> return $ Just cdef
                 ; _ -> do IfServer.nack "ERROR" [ "OpenCnect: no open" ]
                           return Nothing
                 }
     ( towhdls, frowhdls ) <- case cnectdef of
                              { Just (CnectDef _eworld ClientSocket conndefs)
                                  -> lift $ lift $ openCnectClientSockets conndefs
                              ; Just (CnectDef _eworld ServerSocket conndefs)
                                  -> lift $ lift $ openCnectServerSockets conndefs
                              ; Nothing
                                  -> do IfServer.nack "ERROR" [ "OpenCnect: no open" ]
                                        return  ( [], [] )
                              }

     towChan     <- lift $ lift newChan
     frowChan    <- lift $ lift newChan
     towThread   <- lift $ lift $ forkIO $ towChanThread towChan
     frowThreads <- sequence [ lift $ lift $ forkIO $ frowChanThread h frowChan
                             | ConnHfroW _ h _ _ <- frowhdls
                             ]
     modify $ \env -> env { IOS.tow  = ( Just towChan,  Just towThread, towhdls  )
                          , IOS.frow = ( Just frowChan, frowThreads,    frowhdls )
                          }

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
openCnectClientSockets conndefs = do
     let tofrosocks =  [ (ctow, vars', vexp, cfrow, var', vexps, htow, ptow)
                       | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                       , ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
                       , htow == hfrow, ptow == pfrow
                       ]
         tosocks    =  [ (ctow, vars', vexp, htow, ptow)
                       | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                       , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                       ]
         frosocks   =  [ (cfrow, var', vexps, hfrow, pfrow)
                       | ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
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
     let towhdls  = [ ConnHtoW ctow c vars' vexp
                    | ( (ctow, vars', vexp, _, _, _, _, _), c ) <- zip tofrosocks tofroConns
                    ] ++
                    [ ConnHtoW ctow c vars' vexp
                    | ( (ctow, vars', vexp, _, _), c ) <- zip tosocks toConns
                    ]

         frowhdls = [ ConnHfroW cfrow c var' vexps
                    | ( (_, _, _, cfrow, var', vexps, _, _), c ) <- zip tofrosocks tofroConns
                    ]++
                    [ ConnHfroW cfrow c var' vexps
                    | ( (cfrow, var', vexps, _, _), c ) <- zip frosocks froConns
                    ]
     return ( towhdls, frowhdls )

-- ----------------------------------------------------------------------------------------- --

openCnectServerSockets :: [ConnDef] -> IO ([ConnHandle],[ConnHandle])
openCnectServerSockets conndefs  =  do
     let tofrosocks =  [ (ctow, vars', vexp, cfrow, var', vexps, htow, ptow)
                       | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                       , ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
                       , htow == hfrow , ptow == pfrow
                       ]
         tosocks    =  [ (ctow, vars', vexp, htow, ptow)
                       | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                       , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                       ]
         frosocks   =  [ (cfrow, var', vexps, hfrow, pfrow)
                       | ConnDfroW cfrow hfrow pfrow var' vexps <- conndefs
                       , (hfrow,pfrow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                       ]
     tofroConnsA <- async $ mapConcurrently TVS.acceptOn
                                   [fromInteger prt
                                   | (_, _, _, _, _, _, _, prt) <- tofrosocks
                                   ]
     toConnsA    <- async $ mapConcurrently TVS.acceptOn
                                   [fromInteger prt
                                   | (_, _, _, _, prt) <- tosocks
                                   ]
     froConnsA   <- async $ mapConcurrently TVS.acceptOn
                                   [fromInteger prt
                                   | (_, _, _, _, prt) <- frosocks
                                   ]
     tofroConns <- wait tofroConnsA
     toConns    <- wait toConnsA
     froConns   <- wait froConnsA
     let towConns   = [ ConnHtoW ctow c vars' vexp
                     | ( (ctow, vars', vexp, _, _, _, _, _), c ) <- zip tofrosocks tofroConns
                     ]++
                     [ ConnHtoW ctow c vars' vexp
                     | ( (ctow, vars', vexp, _, _), c ) <- zip tosocks toConns
                     ]
         frowConns  = [ ConnHfroW cfrow c var' vexps
                     | ( (_, _, _, cfrow, var', vexps, _, _), c ) <- zip tofrosocks tofroConns
                     ]++
                     [ ConnHfroW cfrow c var' vexps
                     | ( (cfrow, var', vexps, _, _), c ) <- zip frosocks froConns
                     ]
     return ( towConns, frowConns )

-- ----------------------------------------------------------------------------------------- --
-- close connections

closeSockets :: IOS.IOS ()
closeSockets  =  do
     ( _, towThread,   towConns  ) <- gets IOS.tow
     ( _, frowThreads, frowConns ) <- gets IOS.frow

     lift $ lift $ case towThread of
                     Just thrd -> killThread thrd
                     Nothing   -> return ()
     lift $ lift $ mapM_ killThread frowThreads
     lift $ lift $ mapM_ TVS.close [ c | ConnHtoW  _ c _ _ <- towConns  ]
     lift $ lift $ mapM_ TVS.close [ c | ConnHfroW _ c _ _ <- frowConns ]

     modify $ \env -> env { IOS.tow  = ( Nothing, Nothing, [] )
                          , IOS.frow = ( Nothing, [],      [] )
                          }
     return ()


-- ----------------------------------------------------------------------------------------- --
-- putSocket :  try to do output to world on time, or observe earlier input (no quiescence)

putSocket :: IOS.EnvS -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action

putSocket envs act@Act{} =
     let ( Just towChan, _,  _ )  = IOS.tow envs
         ( Just frowChan, _,  _ ) = IOS.frow envs
         ioTime                   = case Map.lookup "param_Sut_ioTime" (IOS.params envs) of
                                      Nothing      -> 10                -- default 10 msec
                                      Just (val,_) -> read val
      in do sact <- EnDecode.encode envs act
            obs  <- lift $ timeout (ioTime*1000) (readChan frowChan)       -- timeout in musec
            case obs of
              Nothing         -> do lift $ writeChan towChan sact
                                    return act
              Just SActQui    -> do lift $ writeChan towChan sact
                                    return act
              Just (SAct h s) -> EnDecode.decode envs (SAct h s)

putSocket envs ActQui =
     let ( Just towChan, _,  _ )  = IOS.tow envs
         ( Just frowChan, _,  _ ) = IOS.frow envs
         deltaTime                = case Map.lookup "param_Sut_deltaTime" (IOS.params envs) of
                                      Nothing      -> 2000                -- default 2 sec
                                      Just (val,_) -> read val
         ioTime                   = case Map.lookup "param_Sut_ioTime" (IOS.params envs) of
                                      Nothing      -> 10                -- default 10 msec
                                      Just (val,_) -> read val
      in do sact <- EnDecode.encode envs ActQui
            obs <- lift $ timeout (ioTime*1000) (readChan frowChan)
            case obs of
              Nothing         -> do lift $ threadDelay (deltaTime*1000)
                                    lift $ writeChan towChan sact
                                    return ActQui
              Just SActQui    -> do lift $ threadDelay (deltaTime*1000)
                                    lift $ writeChan towChan sact
                                    return ActQui
              Just (SAct h s) -> EnDecode.decode envs (SAct h s)

-- ----------------------------------------------------------------------------------------- --
-- getSocket :  observe input from world, or observe quiescence

getSocket :: IOS.EnvS -> IOC.IOC TxsDDefs.Action
getSocket envs =
     let ( Just frowChan, _,  _ ) = IOS.frow envs
         deltaTime                = case Map.lookup "param_Sut_deltaTime" (IOS.params envs) of
                                      Nothing      -> 2000                -- default 2 sec
                                      Just (val,_) -> read val
      in do obs <- lift $ timeout (deltaTime*1000) (readChan frowChan)
            case obs of
              Nothing         -> return ActQui
              Just SActQui    -> return ActQui
              Just (SAct h s) -> EnDecode.decode envs (SAct h s)


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

