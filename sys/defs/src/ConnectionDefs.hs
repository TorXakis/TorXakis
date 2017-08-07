{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ConnectionDefs
where

import GHC.Generics (Generic)
import Control.DeepSeq

import ChanId
import VarId
import ValExprDefs

-- ----------------------------------------------------------------------------------------- --
-- Connection Definitions

data  CnectType     =  ClientSocket
                     | ServerSocket
     deriving (Eq,Ord,Read,Show, Generic, NFData)



data  ConnDef       =  ConnDtoW  { chan       :: ChanId
                                 , hostname   :: String
                                 , portnr     :: Integer
                                 , vars       :: [VarId]         -- encoding domain
                                 , vexpr      :: VExpr           -- encoding range of String
                                 }
                     | ConnDfroW { chan       :: ChanId
                                 , hostname   :: String
                                 , portnr     :: Integer
                                 , var        :: VarId           -- decoding domain of String
                                 , vexprs     :: [VExpr]         -- decoding range
                                 }
     deriving (Eq,Ord,Read,Show, Generic, NFData)


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
