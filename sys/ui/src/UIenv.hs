{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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

import           Control.Monad.Trans.State.Strict
import qualified Data.Map                         as Map
import           Data.Time
import           System.Console.Haskeline
import           System.IO
import           System.Process


-- | Main monad UI state transformer.
type  UIO a  = InputT (StateT UIenv IO) a

-- | Main UI state environment.
data  UIenv  =  UIenv {
    -- | Server socket handle.
    uiservh     :: Handle
    -- | Input handles for user commands.
    , uihins    :: [Handle]
    -- | Output handle for user responses.
    , uihout    :: Handle
    -- | Handles for external processes.
    , uisystems :: Map.Map String ProcessHandle
    -- | Timers.
    , uitimers  :: Map.Map String UTCTime
    -- | TorXakis parameters with checks
    , uiparams  :: Map.Map String ( String, String -> Bool )
    }

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
