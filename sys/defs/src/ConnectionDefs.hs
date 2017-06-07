{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module ConnectionDefs
where

import ChanId
import VarId
import ValExprDefs

-- ----------------------------------------------------------------------------------------- --
-- Connection Definitions

data  CnectType     =  ClientSocket
                     | ServerSocket
     deriving (Eq,Ord,Read,Show)



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
     deriving (Eq,Ord,Read,Show)


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

