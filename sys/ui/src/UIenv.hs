{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module UIenv

-- ----------------------------------------------------------------------------------------- --
--
-- TorXakis UI Environment (Internal State) Data Type Definitions
--
-- ----------------------------------------------------------------------------------------- --
-- export

( UIO
, UIenv (..)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.IO
import System.Process
import Control.Monad.State
import Data.Time

import qualified Data.Map  as Map


-- ----------------------------------------------------------------------------------------- --
-- UIO :  main monad UI state transformer

type  UIO a  =  StateT UIenv IO a


-- ----------------------------------------------------------------------------------------- --
-- UIenv :  main UI state environment

data  UIenv  =  UIenv { uiservh   :: Handle                 -- server socket handle
                      , uihins    :: [Handle]               -- input handles for user commands
                      , uihout    :: Handle                 -- output handle for user responses
                      , uisystems :: Map.Map String ProcessHandle -- handles for external procs
                      , uitimers  :: Map.Map String UTCTime -- timers
                      , uiparams  :: Map.Map String ( String, String -> Bool )
                                                            -- TorXakis parameters with checks
                      }


-- ----------------------------------------------------------------------------------------- --
-- 
-- ----------------------------------------------------------------------------------------- --

