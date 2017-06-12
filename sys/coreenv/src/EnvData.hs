{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module EnvData

-- ----------------------------------------------------------------------------------------- --
--
-- TorXakis Core Environment (Internal State) Additional Data Definitions
--
-- ----------------------------------------------------------------------------------------- --
-- export

( StateNr         -- state number
, Msg (..)        -- (error) messages
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import from defs
import qualified TxsShow   as TxsShow


-- ----------------------------------------------------------------------------------------- --
-- StatNr :  state numbers


type StateNr  =  Int


-- ----------------------------------------------------------------------------------------- --
-- Msg :  (Error) Messages


data Msg     =   TXS_CORE_SYSTEM_ERROR     { s :: String }
               | TXS_CORE_MODEL_ERROR      { s :: String }
               | TXS_CORE_USER_ERROR       { s :: String }
               | TXS_CORE_RUNTIME_ERROR    { s :: String }
               | TXS_CORE_SYSTEM_WARNING   { s :: String }
               | TXS_CORE_MODEL_WARNING    { s :: String }
               | TXS_CORE_USER_WARNING     { s :: String }
               | TXS_CORE_RUNTIME_WARNING  { s :: String }
               | TXS_CORE_SYSTEM_INFO      { s :: String }
               | TXS_CORE_MODEL_INFO       { s :: String }
               | TXS_CORE_USER_INFO        { s :: String }
               | TXS_CORE_RUNTIME_INFO     { s :: String }
               | TXS_CORE_RESPONSE         { s :: String }
               | TXS_CORE_OK               { s :: String }
               | TXS_CORE_NOK              { s :: String }
               | TXS_CORE_ANY              { s :: String }
     deriving (Eq,Ord,Read,Show)

instance TxsShow.PShow Msg
  where
    pshow msg  =  s msg


-- ----------------------------------------------------------------------------------------- --
-- 
-- ----------------------------------------------------------------------------------------- --

