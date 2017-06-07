{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Cnect

-- ----------------------------------------------------------------------------------------- --
--
-- Control, Input, Output for Connections to the Outside World
-- In  = From Outside World
-- Out = To   Outside World
--
-- Exporting:

( openCnect     --  openCnect :: IOE ()
                --  open socket connections to outside world
, closeCnect    --  closeCnectSockets :: IOE ()
                --  close connections to outside world 
, putToW        --  putToW :: Action -> IOE Action
                --  try to output to world, or observe earlier input (no quiescence)
, getFroW       --  getFroW  :: IOE Action
                --  observe input from world on list of handles, or observe quiescence
)

-- ----------------------------------------------------------------------------------------- --
where

import System.IO
import System.IO.Error
import System.Timeout
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.Chan 
import Network
import GHC.Conc
import Debug.Trace

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import TxsDDefs
import TxsEnv

import Utils
import EnDecode
import NoId

-- ----------------------------------------------------------------------------------------- --
-- open connections


openCnect :: IOE ()
openCnect  =  do
     txsmodus <- gets envmodus
     tdefs    <- gets envtdefs
     cnectid  <- case txsmodus of
                 { Testing  _ cid -> return $ (IdCnect cid)
                 ; Simuling _ cid -> return $ (IdCnect cid)
                 ; _              -> do lift $ hPutStrLn stderr $ "TXS openCnect: No open\n"
                                        return $ (IdNo noId)
                 }
     ( towhdls, frowhdls ) <- case TxsDefs.lookup cnectid tdefs of
                              { Just (DefCnect (CnectDef ClientSocket conndefs))
                                  -> do lift $ openCnectClientSockets conndefs
                              ; Just (DefCnect (CnectDef ServerSocket conndefs))
                                  -> do lift $ openCnectServerSockets conndefs
                              ; _ -> do lift $ hPutStrLn stderr $ "TXS openCnect: No open\n"
                                        return $ ( [], [] )
                              }

     towChan     <- lift $ newChan
     frowChan    <- lift $ newChan
     towThread   <- lift $ forkIO $ towChanThread towChan
     frowThreads <- sequence [ lift $ forkIO $ frowChanThread h frowChan
                             | ConnHfroW _ h _ _ <- frowhdls
                             ]
     modify $ \env -> env { envtow  = ( Just towChan,  Just towThread, towhdls  )
                          , envfrow = ( Just frowChan, frowThreads,    frowhdls )
                          }

-- ----------------------------------------------------------------------------------------- --

towChanThread :: Chan SAction -> IO ()
towChanThread towchan  =  do
     sact <- readChan towchan
     case sact of
     { SAct h s -> do hPutStrLn h s
                      towChanThread towchan
     ; SActQui  -> do towChanThread towchan
     }

-- ----------------------------------------------------------------------------------------- --

frowChanThread :: Handle -> Chan SAction -> IO ()
frowChanThread h frowchan  =  do
     s <- hGetLine h
     writeChan frowchan (SAct h s)
     frowChanThread h frowchan

-- ----------------------------------------------------------------------------------------- --

openCnectClientSockets :: [ConnDef] -> IO ([ConnHandle],[ConnHandle])
openCnectClientSockets conndefs  =  do
     tofrosocks  <- return   [ (ctow, vars, vexp, cfrow, var, vexps, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                             , ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                             , htow == hfrow, ptow == pfrow
                             ]
     tosocks     <- return   [ (ctow, vars, vexp, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                             , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                             ]
     frosocks    <- return   [ (cfrow, var, vexps, hfrow, pfrow)
                             | ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                             , (hfrow,pfrow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                             ]
     tofrohandls <- sequence [ connectTo hst (PortNumber (fromInteger prt))
                             | (ctow, _, _, cfrow, _, _, hst, prt) <- tofrosocks
                             ]
     tohandls    <- sequence [ connectTo hst (PortNumber (fromInteger prt))
                             | (ctow, _, _, hst, prt) <- tosocks
                             ]
     frohandls   <- sequence [ connectTo hst (PortNumber (fromInteger prt))
                             | (cfrow, _, _, hst, prt) <- frosocks
                             ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tofrohandls ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tohandls    ]
     sequence_               [ hSetBuffering h NoBuffering | h <- frohandls   ]
     towhdls     <- return $ [ ConnHtoW ctow h vars vexp
                             | ( (ctow, vars, vexp, cfrow, var, vexps, htow, ptow), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHtoW ctow h vars vexp
                             | ( (ctow, vars, vexp, htow, ptow), h )
                                 <- zip tosocks tohandls
                             ]

     frowhdls    <- return $ [ ConnHfroW cfrow h var vexps
                             | ( (ctow, vars, vexp, cfrow, var, vexps, htow, ptow), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHfroW cfrow h var vexps
                             | ( (cfrow, var, vexps, hfrow, pfrow), h )
                                 <- zip frosocks frohandls
                             ]
     return                  $ ( towhdls, frowhdls )

-- ----------------------------------------------------------------------------------------- --

openCnectServerSockets :: [ConnDef] -> IO ([ConnHandle],[ConnHandle])
openCnectServerSockets conndefs  =  do
     tofrosocks  <- return   [ (ctow, vars, vexp, cfrow, var, vexps, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                             , ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                             , htow == hfrow , ptow == pfrow
                             ]
     tosocks     <- return   [ (ctow, vars, vexp, htow, ptow)
                             | ConnDtoW  ctow  htow  ptow  vars vexp  <- conndefs
                             , (htow,ptow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                             ]
     frosocks    <- return   [ (cfrow, var, vexps, hfrow, pfrow)
                             | ConnDfroW cfrow hfrow pfrow var  vexps <- conndefs
                             , (hfrow,pfrow) `notElem` [(h,p)|(_,_,_,_,_,_,h,p)<-tofrosocks]
                             ]
     tofrolistns <- sequence [ listenOn (PortNumber (fromInteger prt))
                             | (ctow, _, _, cfrow, _, _, hst, prt) <- tofrosocks
                             ]

     tolistns    <- sequence [ listenOn (PortNumber (fromInteger prt))
                             | (ctow, _, _, hst, prt) <- tosocks
                             ]
     frolistns   <- sequence [ listenOn (PortNumber (fromInteger prt))
                             | (cfrow, _, _, hst, prt) <- frosocks
                             ]
     tofrocnctns <- sequence [ accept listn | listn <- tofrolistns ]
     tocnctns    <- sequence [ accept listn | listn <- tolistns ]
     frocnctns   <- sequence [ accept listn | listn <- frolistns ] 
     tofrohandls <-          return $ map frst tofrocnctns
     tohandls    <-          return $ map frst tocnctns
     frohandls   <-          return $ map frst frocnctns
     sequence_               [ hSetBuffering h NoBuffering | h <- tofrohandls ]
     sequence_               [ hSetBuffering h NoBuffering | h <- tohandls    ]
     sequence_               [ hSetBuffering h NoBuffering | h <- frohandls   ]
     towhdls     <- return $ [ ConnHtoW ctow h vars vexp
                             | ( (ctow, vars, vexp, cfrow, var, vexps, htow, ptow), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHtoW ctow h vars vexp
                             | ( (ctow, vars, vexp, htow, ptow), h )
                                 <- zip tosocks tohandls
                             ]
     frowhdls    <- return $ [ ConnHfroW cfrow h var vexps
                             | ( (ctow, vars, vexp, cfrow, var, vexps, htow, ptow), h )
                                 <- zip tofrosocks tofrohandls
                             ]
                          ++ [ ConnHfroW cfrow h var vexps
                             | ( (cfrow, var, vexps, hfrow, pfrow), h )
                                 <- zip frosocks frohandls
                             ]
     return                  $ ( towhdls, frowhdls )


-- ----------------------------------------------------------------------------------------- --
-- close connections


closeCnect :: IOE ()
closeCnect  =  do
     ( towChan,  towThread,   towhdls  ) <- gets envtow
     ( frowChan, frowThreads, frowhdls ) <- gets envfrow

     lift $ case towThread of
            { Just thrd -> killThread thrd
            ; Nothing   -> return $ ()
            }
     lift $ mapM killThread frowThreads
     lift $ mapM hClose [ h | ConnHtoW  _ h _ _ <- towhdls  ]
     lift $ mapM hClose [ h | ConnHfroW _ h _ _ <- frowhdls ]

     modify $ \env -> env { envtow  = ( Nothing, Nothing, [] )
                          , envfrow = ( Nothing, [],      [] )
                          }
     return $ ()


-- ----------------------------------------------------------------------------------------- --
-- putToW :  try to do output to world on time, or observe earlier input (no quiescence)


putToW :: Action -> IOE Action

putToW act@(Act acts)  =  do
     ( Just towChan,  _,  _ ) <- gets envtow
     ( Just frowChan, _,  _ ) <- gets envfrow
     sact                     <- encode act
     param_Sut_ioTime         <- getParam "param_Sut_ioTime"
     obs         <- lift $ timeout ((read param_Sut_ioTime)*1000) (readChan frowChan)
     case obs of
     { Nothing         -> do lift $ writeChan towChan sact
                             return $ act
     ; Just (SActQui)  -> do lift $ writeChan towChan sact
                             return $ act
     ; Just (SAct h s) -> do act' <- decode (SAct h s)
                             return $ act'
     }

putOut (ActQui)  =  do
     ( Just towChan,  _,  _ ) <- gets envtow
     ( Just frowChan, _,  _ ) <- gets envfrow
     sact                     <- encode ActQui
     param_Sut_pollDelay      <- getParam "param_Sut_pollDelay"
     obs         <- lift $ timeout ((read param_Sut_pollDelay)*1000) (readChan frowChan)
     case obs of
     { Nothing         -> do param_Sut_deltaTime <- getParam "param_Sut_deltaTime"
                             lift $ threadDelay ((read param_Sut_deltaTime)*1000)
                             lift $ writeChan towChan sact
                             return $ ActQui
     ; Just (SActQui)  -> do param_Sut_deltaTime <- getParam "param_Sut_deltaTime"
                             lift $ threadDelay ((read param_Sut_deltaTime)*1000)
                             lift $ writeChan towChan sact
                             return $ ActQui
     ; Just (SAct h s) -> do act' <- decode (SAct h s)
                             return $ act'
     }


-- ----------------------------------------------------------------------------------------- --
-- getFroW :  observe input from world, or observe quiescence


getFroW :: IOE Action
getFroW   =  do
     ( Just frowChan, _,  _ ) <- gets envfrow
     param_Sut_deltaTime      <- getParam "param_Sut_deltaTime"
     obs         <- lift $ timeout ((read param_Sut_deltaTime)*1000) (readChan frowChan)
     case obs of
     { Nothing         -> do return $ ActQui
     ; Just (SActQui)  -> do return $ ActQui
     ; Just (SAct h s) -> do act' <- decode (SAct h s)
                             return $ act'
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

