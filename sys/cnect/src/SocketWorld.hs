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

( openSockets   --  :: IOS.IOS ()
                --  open socket connections to outside world
, closeSockets  --  :: IOS.IOS ()
                --  close connections to outside world
, putSocket     --  :: IOS.EnvS -> Int -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
                --  try to output to world, or observe earlier input (no quiescence)
, getSocket     --  :: IOS.EnvS -> Int -> IOC.IOC TxsDDefs.Action
                --  observe input from world on list of handles, or observe quiescence
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.State
import qualified Data.Text                as T
import           System.Timeout

import           Network.TextViaSockets   (Connection)
import qualified Network.TextViaSockets   as TVS

-- import from local
import           EnDecode

-- import from serverenv
import qualified EnvServer                as IOS
import qualified IfServer

-- import from coreenv
import qualified EnvCore                  as IOC

-- import from defs
import           TxsDDefs
import           TxsDefs

-- ----------------------------------------------------------------------------------------- --
--



-- ----------------------------------------------------------------------------------------- --
-- open connections


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
                              { Just (CnectDef ClientSocket conndefs)
                                  -> lift $ lift $ openCnectClientSockets conndefs
                              ; Just (CnectDef ServerSocket conndefs)
                                  -> lift $ lift $ openCnectServerSockets conndefs
                              ; Nothing
                                  -> do IfServer.nack "ERROR" [ "OpenCnect: no open" ]
                                        return  ( [], [] )
                              }

     towChan     <- lift $ lift newChan
     frowChan    <- lift $ lift newChan
     towThread   <- lift $ lift $ forkIO $ towChanThread towChan
     frowThreads <- sequence [ lift $ lift $ forkIO $ frowChanThread conn frowChan
                             | ConnHfroW _ conn _ _ <- frowhdls
                             ]
     modify $ \env -> env { IOS.tow  = ( Just towChan,  Just towThread, towhdls  )
                          , IOS.frow = ( Just frowChan, frowThreads,    frowhdls )
                          }

-- ----------------------------------------------------------------------------------------- --

towChanThread :: Chan SAction -> IO ()
towChanThread towchan  =  do
     sact <- readChan towchan
     case sact of
       SAct conn s -> do TVS.putLineTo conn s
                         towChanThread towchan
       SActQui  -> towChanThread towchan

-- ----------------------------------------------------------------------------------------- --

frowChanThread :: Connection -> Chan SAction -> IO ()
frowChanThread conn frowchan  =  do
     s <- TVS.getLineFrom conn
     writeChan frowchan (SAct conn s)
     frowChanThread conn frowchan

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


-- | putSocket :  try to do output to world on time, or observe earlier input (no quiescence)
putSocket :: Int -> Int -> IOS.EnvS -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
putSocket ioTime _ envs act@Act{} =
     let ( Just towChan,  _, _ ) = IOS.tow  envs
         ( Just frowChan, _, _ ) = IOS.frow envs
      in do sact <- EnDecode.encode envs act
            obs  <- lift $ timeout (ioTime*1000) (readChan frowChan)       -- timeout in musec
            case obs of
              Nothing         -> writeAct towChan sact
              Just SActQui    -> writeAct towChan sact
              Just (SAct h s) -> EnDecode.decode envs (SAct h s)
            where
              writeAct ch sa = do lift $ writeChan ch sa
                                  return act

putSocket ioTime deltaTime envs ActQui =
     let ( Just towChan,  _, _ ) = IOS.tow  envs
         ( Just frowChan, _, _ ) = IOS.frow envs
      in do sact <- EnDecode.encode envs ActQui
            obs <- lift $ timeout (ioTime*1000) (readChan frowChan)
            case obs of
              Nothing         -> writeQui towChan sact
              Just SActQui    -> writeQui towChan sact
              Just (SAct h s) -> EnDecode.decode envs (SAct h s)
            where
              writeQui ch sa = do lift $ threadDelay (deltaTime*1000)
                                  lift $ writeChan ch sa
                                  return ActQui

-- | getSocket :  observe input from world, or observe quiescence
getSocket :: Int -> IOS.EnvS -> IOC.IOC TxsDDefs.Action
getSocket deltaTime envs =
     let ( Just frowChan, _,  _ ) = IOS.frow envs
      in do obs <- lift $ timeout (deltaTime*1000) (readChan frowChan)
            case obs of
              Nothing         -> return ActQui
              Just SActQui    -> return ActQui
              Just (SAct h s) -> EnDecode.decode envs (SAct h s)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
