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

module IfServer

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

import System.IO
import Control.Monad.State

-- import from local
import EnvServer

import qualified EnvCore as IOC


-- ----------------------------------------------------------------------------------------- --
-- server socket communication :  get commands


getCmd :: IOS (String,String)
getCmd  =  do
     servhs' <- gets servhs
     cmdline <- lift $ lift $ hGetLine servhs'
     lift $ lift $ hPutStrLn stderr $ "TXSSERVER CMD >>  " ++ cmdline
     case words cmdline of
       (cmd:args)  -> return ( cmd, unwords args )
       _           -> return ( "NOOP", "" )


-- ----------------------------------------------------------------------------------------- --
-- server socket communication :  put responses


putRsp :: Handle -> String -> IOC.IOC ()
putRsp servhs' rsp  =  do
     lift $ hPutStrLn stderr $ "TXSSERVER RSP <<  " ++ rsp
     lift $ hPutStrLn servhs' $ rsp


-- put messages via handle

hmack :: Handle -> [String] -> IOC.IOC ()
hmack servhs'  xss  =  do
     mapM_ ( (putRsp servhs') . ("MACK " ++) ) ( concat $ map lines xss )


-- put multi/intermediate acknowledgements

mack :: [String] -> IOS ()
mack xss  =  do
     servhs' <- gets servhs
     lift $ mapM_ ( (putRsp servhs') . ("MACK " ++) ) ( concat $ map lines xss )


-- put positive acknowledgement

pack :: String -> [String] -> IOS ()
pack cmd xss  =  do
     servhs' <- gets servhs
     lift $ mapM_ ( (putRsp servhs') . ("MACK " ++) ) ( concat $ map lines xss )
     lift $ putRsp servhs' $ "PACK " ++ cmd


-- put negative acknowledgement

nack :: String -> [String] -> IOS ()
nack cmd xss  =  do
     servhs' <- gets servhs
     lift $ mapM_ ( (putRsp servhs') . ("MACK " ++) ) ( concat $ map lines xss )
     lift $ putRsp servhs' $ "NACK " ++ cmd


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

