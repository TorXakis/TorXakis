{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


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
import           Network.Socket
import           System.IO

import qualified Data.HashMap        as Map

import qualified TorXakis.CoreState             as IOC

import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.SortContext
import           TorXakis.Var
import           TorXakis.ContextValExpr
import           TorXakis.Value
import           TorXakis.PrettyPrint

-- ----------------------------------------------------------------------------------------- --
-- IOS :  torxakis server main state monad transformer


type IOS a = StateT EnvS IOC.IOC a


-- ----------------------------------------------------------------------------------------- --
-- torxakis server state type definitions


data EnvS  = EnvS { host       :: String                    -- ^ host of server client
                  , portNr     :: PortNumber                -- ^ port number of server client
                  , servhs     :: Handle                    -- ^ server socket handle
                  , modus      :: TxsModus                  -- ^ current modus of TXS operation
                  , locVexpCtx :: ContextValExpr            -- ^ local ValExprContext
                  , locVarVals :: VEnv                      -- ^ local variables with values
                  }


envsNone    :: EnvS
envsNone   = EnvS { host         = ""
                  , portNr       = 0
                  , servhs       = stderr
                  , modus        = Noned
                  , locVexpCtx   = TorXakis.ContextValExpr.empty
                  , locVarVals   = VEnv Map.empty
                  }


newtype VEnv = VEnv { toMap :: Map.Map (RefByName VarDef) Value }

instance SortContext c => PrettyPrint c VEnv
  where
     prettyPrint o c a  =  intercalate (fromString "\n")
                                       (map ppElem (Map.toList $ toMap a))
       where
          ppElem :: (RefByName VarDef,Value) -> TxsString
          ppElem (nm,val)  =  TorXakis.Language.concat [ fromText $ TorXakis.Name.toText $ toName nm
                                                       , fromString " = "
                                                       , prettyPrint o c val
                                                       ]


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

