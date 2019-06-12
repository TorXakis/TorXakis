{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --

module TorXakis.IfServer

-- ----------------------------------------------------------------------------------------- --
--
--   Interfaces for TorXakis as a Socket Service
--
-- ----------------------------------------------------------------------------------------- --
 -- export

( getCmd            --  :: IOS (String,String)
, hmack             --  :: Handle -> [String] -> IOC.IOC ()
, mack              --  :: String -> [String] -> IOS ()
, pack              --  :: String -> [String] -> IOS ()
, nack              --  :: String -> [String] -> IOS ()

)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State
import           System.IO

-- import from local
import           TorXakis.ServerState

import qualified TorXakis.CoreState             as IOC

-- ----------------------------------------------------------------------------------------- --
-- server socket communication :  get commands

getCmd :: IOS (String,String)
getCmd = do
     servhs' <- gets servhs
     cmdline <- lift $ lift $ hGetLine servhs'
     lift $ lift $ hPutStrLn stderr $ "TXSSERVER CMD >>  " ++ cmdline
     case words cmdline of
       (cmd:args) -> return ( cmd, unwords args )
       _          -> return ( "NOOP", "" )


-- ----------------------------------------------------------------------------------------- --
-- server socket communication :  put responses


putRsp :: Handle -> String -> IOC.IOC ()
putRsp servhs' rsp = do
     lift $ hPutStrLn stderr $ "TXSSERVER RSP <<  " ++ rsp
     lift $ hPutStrLn servhs' rsp

-- put messages via handle

hmack :: Handle -> [String] -> IOC.IOC ()
hmack servhs' xss = mapM_ ( putRsp servhs' . ("MACK " ++) ) ( concatMap lines xss )


-- put multi/intermediate acknowledgments

mack :: [String] -> IOS ()
mack xss = do
     servhs' <- gets servhs
     lift $ mapM_ ( putRsp servhs' . ("MACK " ++) ) ( concatMap lines xss )


-- put positive acknowledgment

pack :: String -> [String] -> IOS ()
pack cmd xss = do
     servhs' <- gets servhs
     lift $ mapM_ ( putRsp servhs' . ("MACK " ++) ) ( concatMap lines xss )
     lift $ putRsp servhs' $ "PACK " ++ cmd


-- put negative acknowledgement

nack :: String -> [String] -> IOS ()
nack cmd xss = do
     servhs' <- gets servhs
     lift $ mapM_ ( putRsp servhs' . ("MACK " ++) ) ( concatMap lines xss )
     lift $ putRsp servhs' $ "NACK " ++ cmd


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

