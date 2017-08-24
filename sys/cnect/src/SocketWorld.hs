{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

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

import System.IO
-- import System.IO.Error
import System.Timeout
import Control.Monad.State
import Control.Concurrent
import Network
-- import GHC.Conc

-- import qualified Data.Char as Char
-- import qualified Data.List as List
-- import qualified Data.Set  as Set
import qualified Data.Map  as Map

-- import from local
import EnDecode

-- import from serverenv
import qualified EnvServer   as IOS
import qualified IfServer

-- import from coreenv
import qualified EnvCore as IOC

-- import from defs
import qualified Utils
import TxsDefs
import TxsDDefs

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
                                  -> do lift $ lift $ openCnectClientSockets conndefs
                              ; Just (CnectDef ServerSocket conndefs)
                                  -> do lift $ lift $ openCnectServerSockets conndefs
                              ; Nothing 
                                  -> do IfServer.nack "ERROR" [ "OpenCnect: no open" ]
                                        return $ ( [], [] )
                              }

     towChan     <- lift $ lift $ newChan
     frowChan    <- lift $ lift $ newChan
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
       SAct h s -> do hPutStrLn h s
                      towChanThread towchan
       SActQui  -> do towChanThread towchan

-- ----------------------------------------------------------------------------------------- --

frowChanThread :: Handle -> Chan SAction -> IO ()
frowChanThread h frowchan  =  do
     s <- hGetLine h
     writeChan frowchan (SAct h s)
     frowChanThread h frowchan

-- ----------------------------------------------------------------------------------------- --

openCnectClientSockets :: [ConnDef] -> IO ([ConnHandle],[ConnHandle])
openCnectClientSockets conndefs  =  do
     tofrosocks  <- return   [ (ctow, vars', vexp, cfrow, var', vexps, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                             , ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
                             , htow == hfrow, ptow == pfrow
                             ]
     tosocks     <- return   [ (ctow, vars', vexp, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                             , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                             ]
     frosocks    <- return   [ (cfrow, var', vexps, hfrow, pfrow)
                             | ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
                             , (hfrow,pfrow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                             ]
     tofrohandls <- sequence [ connectTo hst (PortNumber (fromInteger prt))
                             | (_, _, _, _, _, _, hst, prt) <- tofrosocks
                             ]
     tohandls    <- sequence [ connectTo hst (PortNumber (fromInteger prt))
                             | (_, _, _, hst, prt) <- tosocks
                             ]
     frohandls   <- sequence [ connectTo hst (PortNumber (fromInteger prt))
                             | (_, _, _, hst, prt) <- frosocks
                             ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tofrohandls ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tohandls    ]
     sequence_               [ hSetBuffering h NoBuffering | h <- frohandls   ]
     towhdls     <- return $ [ ConnHtoW ctow h vars' vexp
                             | ( (ctow, vars', vexp, _, _, _, _, _), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHtoW ctow h vars' vexp
                             | ( (ctow, vars', vexp, _, _), h )
                                 <- zip tosocks tohandls
                             ]

     frowhdls    <- return $ [ ConnHfroW cfrow h var' vexps
                             | ( (_, _, _, cfrow, var', vexps, _, _), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHfroW cfrow h var' vexps
                             | ( (cfrow, var', vexps, _, _), h )
                                 <- zip frosocks frohandls
                             ]
     return                  $ ( towhdls, frowhdls )

-- ----------------------------------------------------------------------------------------- --

openCnectServerSockets :: [ConnDef] -> IO ([ConnHandle],[ConnHandle])
openCnectServerSockets conndefs  =  do
     tofrosocks  <- return   [ (ctow, vars', vexp, cfrow, var', vexps, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                             , ConnDfroW cfrow hfrow pfrow var'  vexps <- conndefs
                             , htow == hfrow , ptow == pfrow
                             ]
     tosocks     <- return   [ (ctow, vars', vexp, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars' vexp  <- conndefs
                             , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                             ]
     frosocks    <- return   [ (cfrow, var', vexps, hfrow, pfrow)
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
     tofrohandls <-          return $ map Utils.frst tofrocnctns
     tohandls    <-          return $ map Utils.frst tocnctns
     frohandls   <-          return $ map Utils.frst frocnctns
     sequence_               [ hSetBuffering h NoBuffering | h <- tofrohandls ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tohandls    ]
     sequence_               [ hSetBuffering h NoBuffering | h <- frohandls   ]
     towhdls     <- return $ [ ConnHtoW ctow h vars' vexp
                             | ( (ctow, vars', vexp, _, _, _, _, _), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHtoW ctow h vars' vexp
                             | ( (ctow, vars', vexp, _, _), h )
                                 <- zip tosocks tohandls
                             ]
     frowhdls    <- return $ [ ConnHfroW cfrow h var' vexps
                             | ( (_, _, _, cfrow, var', vexps, _, _), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHfroW cfrow h var' vexps
                             | ( (cfrow, var', vexps, _, _), h )
                                 <- zip frosocks frohandls
                             ]
     return                  $ ( towhdls, frowhdls )


-- ----------------------------------------------------------------------------------------- --
-- close connections


closeSockets :: IOS.IOS ()
closeSockets  =  do
     ( _, towThread,   towhdls  ) <- gets IOS.tow
     ( _, frowThreads, frowhdls ) <- gets IOS.frow

     lift $ lift $ case towThread of
                   { Just thrd -> killThread thrd
                   ; Nothing   -> return $ ()
                   }
     lift $ lift $ mapM_ killThread frowThreads
     lift $ lift $ mapM_ hClose [ h | ConnHtoW  _ h _ _ <- towhdls  ]
     lift $ lift $ mapM_ hClose [ h | ConnHfroW _ h _ _ <- frowhdls ]

     modify $ \env -> env { IOS.tow  = ( Nothing, Nothing, [] )
                          , IOS.frow = ( Nothing, [],      [] )
                          }
     return $ ()


-- ----------------------------------------------------------------------------------------- --
-- putSocket :  try to do output to world on time, or observe earlier input (no quiescence)


putSocket :: IOS.EnvS -> TxsDDefs.Action -> IOC.IOC TxsDDefs.Action

putSocket envs act@Act{}  =  do
     let ( Just towChan, _,  _ )  = IOS.tow envs
         ( Just frowChan, _,  _ ) = IOS.frow envs
         ioTime                   = case Map.lookup "param_Sut_ioTime" (IOS.params envs) of
                                    { Nothing          -> 10                -- default 10 msec
                                    ; Just (val,_)     -> read val
                                    }
      in do sact <- EnDecode.encode envs act
            obs  <- lift $ timeout (ioTime*1000) (readChan frowChan)       -- timeout in musec
            case obs of
              Nothing         -> do lift $ writeChan towChan sact
                                    return $ act
              Just (SActQui)  -> do lift $ writeChan towChan sact
                                    return $ act
              Just (SAct h s) -> do act' <- EnDecode.decode envs (SAct h s)
                                    return $ act'
            
putSocket envs ActQui  =  do
     let ( Just towChan, _,  _ )  = IOS.tow envs
         ( Just frowChan, _,  _ ) = IOS.frow envs
         deltaTime                = case Map.lookup "param_Sut_deltaTime" (IOS.params envs) of
                                    { Nothing          -> 2000                -- default 2 sec 
                                    ; Just (val,_)     -> read val
                                    }
         ioTime                   = case Map.lookup "param_Sut_ioTime" (IOS.params envs) of
                                    { Nothing          -> 10                -- default 10 msec
                                    ; Just (val,_)     -> read val
                                    }
      in do sact <- EnDecode.encode envs ActQui
            obs <- lift $ timeout (ioTime*1000) (readChan frowChan)
            case obs of
            { Nothing         -> do lift $ threadDelay (deltaTime*1000)
                                    lift $ writeChan towChan sact
                                    return $ ActQui
            ; Just (SActQui)  -> do lift $ threadDelay (deltaTime*1000)
                                    lift $ writeChan towChan sact
                                    return $ ActQui
            ; Just (SAct h s) -> do act' <- EnDecode.decode envs (SAct h s)
                                    return $ act'
            }


-- ----------------------------------------------------------------------------------------- --
-- getSocket :  observe input from world, or observe quiescence


getSocket :: IOS.EnvS -> IOC.IOC TxsDDefs.Action
getSocket envs  =  do
     let ( Just frowChan, _,  _ ) = IOS.frow envs
         deltaTime                = case Map.lookup "param_Sut_deltaTime" (IOS.params envs) of
                                    { Nothing      -> 2000                -- default 2 sec 
                                    ; Just (val,_) -> read val
                                    }
      in do obs <- lift $ timeout (deltaTime*1000) (readChan frowChan)
            case obs of
            { Nothing         -> do return $ ActQui
            ; Just (SActQui)  -> do return $ ActQui
            ; Just (SAct h s) -> do act' <- EnDecode.decode envs (SAct h s)
                                    return $ act'
            }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
