{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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

import           Control.Monad.State
import           Data.Time
import           System.IO

import           Data.String.Utils

import qualified Data.Map            as Map

import           UIenv


-- ----------------------------------------------------------------------------------------- --
-- client to server socket communication

doCmd :: String -> String -> UIO ()
doCmd cmd cargs  =  do
     hs     <- lift $ gets uiservh
     let cargs' = if null (dropWhile (== ' ') cargs) then "" else " " ++ cargs
     liftIO $ hPutStrLn hs $ cmd ++ cargs'
     doRsp cmd

-- ----------------------------------------------------------------------------------------- --

doRsp :: String -> UIO ()
doRsp cmd  =  do
     hs      <- lift $ gets uiservh
     rspline <- liftIO $ hGetLine hs
     case words rspline of
        []                            -> doRsp cmd
        ("MACK":"ERROR":rargs)        -> do putErr $ unwords rargs
                                            doRsp cmd
        ("MACK":rargs)                -> do putOut $ unwords rargs
                                            doRsp cmd
        ("PACK":[rsp])   | cmd == rsp -> return ()
        ("NACK":[rsp])   | cmd == rsp -> return ()
        ("NACK":"ERROR":rargs)        -> putErr $ unwords rargs
        _                             -> putErr $ "unknown txsserver response: " ++ rspline


-- ----------------------------------------------------------------------------------------- --
-- user standard IO communication

setOut :: String -> IOMode -> UIO ()
setOut fname iomode  =  do
     hOut <- lift $ gets uihout
     case (hOut,fname) of
       (h,f) | h == stdout && f == "" -> return ()
             | h == stdout && f /= "" -> do hout <- liftIO $ openFile f iomode
                                            liftIO $ hSetBuffering hout NoBuffering
                                            lift $ modify ( \env -> env { uihout = hout } )
             | h /= stdout && f == "" -> do liftIO $ hClose h
                                            lift $ modify ( \env -> env { uihout = stdout } )
             | h /= stdout && f /= "" -> do liftIO $ hClose h
                                            hout <- liftIO $ openFile f iomode
                                            liftIO $ hSetBuffering hout NoBuffering
                                            lift $ modify ( \env -> env { uihout = hout } )
       (_,_)                          -> error $ "setOut: Impossible: prevent warning\n" ++
                                                 "Pattern match(es) are non-exhaustive\n" ++
                                                 "In a case alternative: Patterns not matched: (_, _)"

-- ----------------------------------------------------------------------------------------- --

putOut :: String -> UIO ()
putOut s  =  do
     hout <- lift$ gets uihout
     let pre = if hout == stdout then "TXS >>  " else ""
      in liftIO $ hPutStrLn hout $ pre ++ s

-- ----------------------------------------------------------------------------------------- --

putErr :: String -> UIO ()
putErr s  = liftIO $ hPutStrLn stderr $ "TXS Error >>  " ++ s


-- ----------------------------------------------------------------------------------------- --
-- time and timers

showTime :: UIO String
showTime  =  do
     timezone <- liftIO getCurrentTimeZone
     now      <- liftIO getCurrentTime
     return $ "Time = " ++ replace ":" "-" ( show $ utcToLocalTime timezone now )

-- ----------------------------------------------------------------------------------------- --

doTimer :: String -> UIO String
doTimer timername  =  do
     timezone <- liftIO getCurrentTimeZone
     now      <- liftIO getCurrentTime
     timers   <- lift $ gets uitimers
     case Map.lookup timername timers of
        Nothing   -> do lift $ modify ( \env -> env { uitimers = Map.insert timername now timers } )
                        return $ "TXS >> Timer " ++ show timername ++ " start = " ++
                                show ( utcToLocalTime timezone now )
        Just prev -> do when (timername /= "global") $
                            lift $ modify (\env -> env { uitimers = Map.delete timername timers })
                        let period = diffUTCTime now prev
                        return $ "TXS >> Timer " ++ show timername ++ " stop = " ++
                                show ( utcToLocalTime timezone now ) ++
                                " -- Duration = " ++ show period

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
