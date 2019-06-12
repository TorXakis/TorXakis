{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module TorXakis.ServerState

-- ----------------------------------------------------------------------------------------- --
--
-- TorXakis Server Environment (Internal State) Data Type Definitions
--
-- ----------------------------------------------------------------------------------------- --
-- export

( IOS             -- type IOS a = StateT EnvS IOC a
                  -- torxakis server main state monad transformer
, EnvS     (..)   -- torxakis server state
, envsNone        -- torxakis server initial state
, TxsModus (..)   -- torxakis server modus
, isNoned         -- isXX :: TxsModus -> Bool
, isIdled         -- check whether torxakis modus is XX
, isInited        --
, isGtNoned       -- isGtXX :: TxsModus -> Bool
, isGtIdled       -- check whether torxakis modus is greater (further) than XX
, isGtInited      --
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State
import           Network
import           System.IO

import qualified Data.Map            as Map

import qualified TorXakis.CoreState             as IOC

import           TorXakis.Var
import           TorXakis.Value

-- ----------------------------------------------------------------------------------------- --
-- IOS :  torxakis server main state monad transformer


type IOS a = StateT EnvS IOC.IOC a


-- ----------------------------------------------------------------------------------------- --
-- torxakis server state type definitions


data EnvS  = EnvS { host    :: String                    -- ^ host of server client
                  , portNr  :: PortNumber                -- ^ port number of server client
                  , servhs  :: Handle                    -- ^ server socket handle
                  , modus   :: TxsModus                  -- ^ current modus of TXS operation
                  , locvars :: VEnv                      -- ^ local variables
                  }


envsNone    :: EnvS
envsNone   = EnvS { host      = ""
                  , portNr    = 0
                  , servhs    = stderr
                  , modus     = Noned
                  , locvars   = Map.empty
                  }


type VEnv = Map.Map VarDef (Maybe Value)


-- ----------------------------------------------------------------------------------------- --
-- Txs Modus


data  TxsModus = Noned
                 | Idled
                 | Inited

isNoned, isIdled, isInited        :: TxsModus -> Bool
isGtNoned, isGtIdled, isGtInited  :: TxsModus -> Bool

isNoned Noned = True
isNoned _     = False
isIdled Idled = True
isIdled _     = False
isInited Inited = True
isInited _      = False
isGtNoned  m             = not (isNoned m)
isGtIdled  m             = isGtNoned m && not (isIdled m)
isGtInited m             = isGtIdled m && not (isInited m)


-- ----------------------------------------------------------------------------------------- --

