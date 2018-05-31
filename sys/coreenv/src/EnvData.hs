{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveGeneric #-}
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

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)

-- import from defs
import           TxsDDefs     (Action)
import qualified TxsShow

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
               | AnAction                  { step :: Int, act :: Action } -- ^ An action was performed.
     deriving (Eq,Ord,Read,Show,Generic)

instance ToJSON Msg
instance FromJSON Msg

instance TxsShow.PShow Msg
  where
    pshow (AnAction stp a) = TxsShow.showN stp 6 ++ ": " ++ TxsShow.pshow a
    pshow x                = s x

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
