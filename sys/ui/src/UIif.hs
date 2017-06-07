{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module UIif

-- ----------------------------------------------------------------------------------------- --
--
--   Interfaces for TorXakis Line-Oriented User Interface Client
--
-- ----------------------------------------------------------------------------------------- --
-- export

( doCmd        --  doCmd :: String -> String -> UIO ()
, setOut       --  setOut :: String -> IOMode -> UIO ()
, putOut       --  putOut :: String -> UIO ()
, putErr       --  putErr  :: String -> UIO ()
, showTime     --  showTime :: IOE String
, doTimer      --  doTimer :: String -> IOE String
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.IO
import Control.Monad.State
import Data.Time

import Data.String.Utils

import qualified Data.Map  as Map
 
import UIenv


-- ----------------------------------------------------------------------------------------- --
-- client to server socket communication

doCmd :: String -> String -> UIO ()
doCmd cmd cargs  =  do
     hs     <- gets uiservh
     let cargs' = if null (dropWhile (== ' ') cargs) then "" else " " ++ cargs
     lift $ hPutStrLn hs $ cmd ++ cargs'
     doRsp cmd

-- ----------------------------------------------------------------------------------------- --

doRsp :: String -> UIO ()
doRsp cmd  =  do
     hs      <- gets uiservh
     rspline <- lift $ hGetLine hs
     case words rspline of
        []                              -> doRsp cmd
        ("PACK":rsp:rargs) | cmd == rsp -> do putOut $ unwords rargs
                                              doRsp cmd
        ("MACK":rsp:rargs) | cmd == rsp -> do putErr $ unwords rargs
                                              doRsp cmd
        ("FACK":rsp:rargs) | cmd == rsp -> putOut $ unwords rargs
        ("NACK":rsp:rargs) | cmd == rsp -> putErr $ unwords rargs
        _                               -> putErr $ "unknown txsserver response: " ++ rspline
     

-- ----------------------------------------------------------------------------------------- --
-- user standard IO communication

setOut :: String -> IOMode -> UIO ()
setOut fname iomode  =  do
     hOut <- gets uihout
     case (hOut,fname) of
       (h,f) | h == stdout && f == "" -> return ()
             | h == stdout && f /= "" -> do hout <- lift $ openFile f iomode
                                            lift $ hSetBuffering hout NoBuffering
                                            modify ( \env -> env { uihout = hout } )
             | h /= stdout && f == "" -> do lift $ hClose h
                                            modify ( \env -> env { uihout = stdout } )
             | h /= stdout && f /= "" -> do lift $ hClose h
                                            hout <- lift $ openFile f iomode
                                            lift $ hSetBuffering hout NoBuffering
                                            modify ( \env -> env { uihout = hout } )
     
-- ----------------------------------------------------------------------------------------- --

putOut :: String -> UIO ()
putOut s  =  do
     hout <- gets uihout
     lift $ hPutStrLn hout $ "TXS >>  " ++ s

-- ----------------------------------------------------------------------------------------- --

putErr :: String -> UIO ()
putErr s  = lift $ hPutStrLn stderr $ "TXS Error >>  " ++ s


-- ----------------------------------------------------------------------------------------- --
-- time and timers

showTime :: UIO String
showTime  =  do
     timezone <- lift getCurrentTimeZone
     now      <- lift getCurrentTime
     return $ "Time = " ++ replace ":" "-" ( show $ utcToLocalTime timezone now )

-- ----------------------------------------------------------------------------------------- --
     
doTimer :: String -> UIO String
doTimer timername  =  do
     timezone <- lift getCurrentTimeZone
     now      <- lift getCurrentTime
     timers   <- gets uitimers
     case Map.lookup timername timers of
        Nothing   -> do modify ( \env -> env { uitimers = Map.insert timername now timers } )
                        return $ "TXS >> Timer " ++ show timername ++ " start = " ++
                                show ( utcToLocalTime timezone now )
        Just prev -> do when (timername /= "global") $
                            modify (\env -> env { uitimers = Map.delete timername timers })
                        let period = diffUTCTime now prev
                        return $ "TXS >> Timer " ++ show timername ++ " stop = " ++
                                show ( utcToLocalTime timezone now ) ++
                                " -- Duration = " ++ show period

-- ----------------------------------------------------------------------------------------- --
-- 
-- ----------------------------------------------------------------------------------------- --