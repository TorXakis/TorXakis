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

(
-- startSockWorld   --
-- stopSockWorld    --
-- putSockWorld     --  :: IOS.EnvS -> Int -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
                    --  try to output to world, or observe earlier input (no quiescence)
-- getSockWorld     --  :: IOS.EnvS -> Int -> IOC.IOC TxsDDefs.Action
                    --  observe input from world on list of handles, or observe quiescence
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           System.IO
import           System.Process
import           Control.Concurrent
import           Control.Monad.State
import qualified Data.Text           as T
import           Network
import           System.Timeout
-- import GHC.Conc

-- import qualified Data.Char as Char
-- import qualified Data.List as List
-- import qualified Data.Set  as Set
import qualified Data.Map            as Map

-- import from local
import           EnDecode

-- import from serverenv
import qualified EnvServer           as IOS
import qualified IfServer

-- import from coreenv
import qualified EnvCore             as IOC

-- import from defs
import           TxsDDefs
import           TxsDefs
import qualified Utils


-- ----------------------------------------------------------------------------------------- --
-- socket world


{-
data SockWorld = SockWorld { tow    :: ( Maybe (Chan TxsDDefs.SAction)
                                       , Maybe ThreadId
                                       , [TxsDDefs.ConnHandle]
                                       )                   -- ^ connections to world
                           , frow   :: ( Maybe (Chan TxsDDefs.SAction)
                                       , [ThreadId]
                                       , [TxsDDefs.ConnHandle]
                                       )                   -- ^ connections from world
                           }
-}


instance IOC.EWorld IOS.EnvS
  where
     startW  = startSockWorld
     stopW   = stopSockWorld
     putToW  = putSockWorld
     getFroW = getSockWorld
 

-- ----------------------------------------------------------------------------------------- --
-- socketworld as eworld


startSockWorld :: IOS.EnvS -> IOC.IOC IOS.EnvS
startSockWorld w  =  do
     (_,w') <- runStateT openSockets w
     return w'


stopSockWorld  :: IOS.EnvS -> IOC.IOC IOS.EnvS
stopSockWorld w  =  do
     (_,w') <- runStateT closeSockets w
     return w'


-- ----------------------------------------------------------------------------------------- --
-- open connections


openSockets :: IOS.IOS ()
openSockets  =  do
     txsmodus <- gets IOS.modus
     IfServer.mack [ "X1: " ++ (show txsmodus) ]
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
       SAct h s -> do hPutStrLn h (T.unpack s)
                      towChanThread towchan
       SActQui  -> towChanThread towchan

-- ----------------------------------------------------------------------------------------- --

frowChanThread :: Handle -> Chan SAction -> IO ()
frowChanThread h frowchan  =  do
     s <- T.pack <$> hGetLine h
     writeChan frowchan (SAct h s)
     frowChanThread h frowchan

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
     tofrohandls <- sequence [ connectTo (T.unpack hst) (PortNumber (fromInteger prt))
                             | (_, _, _, _, _, _, hst, prt) <- tofrosocks
                             ]
     tohandls    <- sequence [ connectTo (T.unpack hst)(PortNumber (fromInteger prt))
                             | (_, _, _, hst, prt) <- tosocks
                             ]
     frohandls   <- sequence [ connectTo (T.unpack hst) (PortNumber (fromInteger prt))
                             | (_, _, _, hst, prt) <- frosocks
                             ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tofrohandls ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tohandls    ]
     sequence_               [ hSetBuffering h NoBuffering | h <- frohandls   ]
     let towhdls  = [ ConnHtoW ctow h vars' vexp
                    | ( (ctow, vars', vexp, _, _, _, _, _), h ) <- zip tofrosocks tofrohandls
                    ] ++
                    [ ConnHtoW ctow h vars' vexp
                    | ( (ctow, vars', vexp, _, _), h ) <- zip tosocks tohandls
                    ]

         frowhdls = [ ConnHfroW cfrow h var' vexps
                    | ( (_, _, _, cfrow, var', vexps, _, _), h ) <- zip tofrosocks tofrohandls
                    ]++
                    [ ConnHfroW cfrow h var' vexps
                    | ( (cfrow, var', vexps, _, _), h ) <- zip frosocks frohandls
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
     tofrolistns <- sequence [ listenOn (PortNumber (fromInteger prt))
                             | (_, _, _, _, _, _, _, prt) <- tofrosocks
                             ]
     tolistns    <- sequence [ listenOn (PortNumber (fromInteger prt))
                             | (_, _, _, _, prt) <- tosocks
                             ]
     frolistns   <- sequence [ listenOn (PortNumber (fromInteger prt))
                             | (_, _, _, _, prt) <- frosocks
                             ]
     tofrocnctns <- sequence [ accept listn | listn <- tofrolistns ]
     tocnctns    <- sequence [ accept listn | listn <- tolistns ]
     frocnctns   <- sequence [ accept listn | listn <- frolistns ]
     let tofrohandls = map Utils.frst tofrocnctns
         tohandls    = map Utils.frst tocnctns
         frohandls   = map Utils.frst frocnctns
     sequence_       [ hSetBuffering h NoBuffering | h <- tofrohandls ]
     sequence_       [ hSetBuffering h NoBuffering | h <- tohandls    ]
     sequence_       [ hSetBuffering h NoBuffering | h <- frohandls   ]
     let towhdls   = [ ConnHtoW ctow h vars' vexp
                     | ( (ctow, vars', vexp, _, _, _, _, _), h ) <- zip tofrosocks tofrohandls
                     ]++
                     [ ConnHtoW ctow h vars' vexp
                     | ( (ctow, vars', vexp, _, _), h ) <- zip tosocks tohandls
                     ]
         frowhdls  = [ ConnHfroW cfrow h var' vexps
                     | ( (_, _, _, cfrow, var', vexps, _, _), h ) <- zip tofrosocks tofrohandls
                     ]++
                     [ ConnHfroW cfrow h var' vexps
                     | ( (cfrow, var', vexps, _, _), h ) <- zip frosocks frohandls
                     ]
     return ( towhdls, frowhdls )

-- ----------------------------------------------------------------------------------------- --
-- close connections

closeSockets :: IOS.IOS ()
closeSockets  =  do
     ( _, towThread,   towhdls  ) <- gets IOS.tow
     ( _, frowThreads, frowhdls ) <- gets IOS.frow

     lift $ lift $ mapM_ hClose [ h | ConnHtoW  _ h _ _ <- towhdls  ]
     lift $ lift $ mapM_ hClose [ h | ConnHfroW _ h _ _ <- frowhdls ]

     lift $ lift $ case towThread of
                     Just thrd -> killThread thrd
                     Nothing   -> return ()
     lift $ lift $ mapM_ killThread frowThreads

     modify $ \env -> env { IOS.tow  = ( Nothing, Nothing, [] )
                          , IOS.frow = ( Nothing, [],      [] )
                          }
     return ()


-- ----------------------------------------------------------------------------------------- --
-- putSockWorld :  try to do output to world on time, or observe earlier input (no quiescence)

putSockWorld :: IOS.EnvS -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action

putSockWorld envs act@Act{} =
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

putSockWorld envs ActQui =
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
-- getSockwWorld :  observe input from world, or observe quiescence

getSockWorld :: IOS.EnvS -> IOC.IOC TxsDDefs.Action
getSockWorld envs =
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
