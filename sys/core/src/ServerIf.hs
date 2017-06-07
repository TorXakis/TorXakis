{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module ServerIf

-- ----------------------------------------------------------------------------------------- --
--
--   Interfaces for TorXakis as a Socket Service
--
-- ----------------------------------------------------------------------------------------- --
 -- export

( getCmd        --  getCmd :: IOE (String,String)
, pack          --  pack :: String -> String -> IOE ()
, mack          --  mack :: String -> String -> IOE ()
, fack          --  fack :: String -> String -> IOE ()
, nack          --  nack :: String -> String -> IOE ()
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.IO
import Network
import Control.Monad.State

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map
 
import TxsEnv


-- ----------------------------------------------------------------------------------------- --
-- server socket communication


getCmd :: IOE (String,String)
getCmd  =  do
     servhs  <- gets envservhs
     cmdline <- lift $ hGetLine servhs
     lift $ hPutStrLn stderr $ "TXSSERVER CMD >>  " ++ cmdline
     case words cmdline of
     { (cmd:args)  -> return $ ( cmd, unwords args )
     ; _           -> return $ ( "NOOP", "" )
     }


putRsp :: String -> IOE ()
putRsp rsp  =  do
     servhs  <- gets envservhs
     lift $ hPutStrLn stderr $ "TXSSERVER RSP <<  " ++ rsp
     lift $ hPutStrLn servhs $ rsp


pack :: String -> String -> IOE ()
pack cmd s  =  do
     case lines s of
     { []     -> do putRsp $ "PACK " ++ cmd
     ; [l]    -> do putRsp $ "PACK " ++ cmd ++ " " ++ l
     ; (l:ll) -> do mapM putRsp $ map (("PACK " ++ cmd ++ " ") ++) (l:ll)
                    return $ ()
     }


mack :: String -> String -> IOE ()
mack cmd s  =  do
     case lines s of
     { []     -> do putRsp $ "MACK " ++ cmd
     ; [l]    -> do putRsp $ "MACK " ++ cmd ++ " " ++ l
     ; (l:ll) -> do mapM putRsp $ map (("MACK " ++ cmd ++ " ") ++) (l:ll)
                    return $ ()
     }


fack :: String -> String -> IOE ()
fack cmd s  =  do
     case lines s of
     { []     -> do putRsp $ "FACK " ++ cmd
     ; [l]    -> do putRsp $ "FACK " ++ cmd ++ " " ++ l
     ; (l:ll) -> do mapM putRsp $ map (("PACK " ++ cmd ++ " ") ++) (init (l:ll))
                    putRsp $ "FACK " ++ cmd ++ " " ++ (last (l:ll))
     }


nack :: String -> String -> IOE ()
nack cmd s  =  do
     case lines s of
     { []     -> do putRsp $ "NACK " ++ cmd
     ; [l]    -> do putRsp $ "NACK " ++ cmd ++ " " ++ l
     ; (l:ll) -> do mapM putRsp $ map (("MACK " ++ cmd ++ " ") ++) (init (l:ll))
                    putRsp $ "NACK " ++ cmd ++ " " ++ (last (l:ll))
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

