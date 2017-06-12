{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module TxsDDefs

where

import System.IO
import qualified Data.Set as Set

import TxsDefs(ChanId, Const, VarId, VExpr)
import TxsShow
-- ----------------------------------------------------------------------------------------- --
-- Const  :  values (for now: VExpr)
-- Action :  trie/primer valued actions


data  Action   =  Act     ( Set.Set (ChanId,[Const]) )
                | ActQui
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
-- SAction :  string encoded actions


data  SAction       =  SAct     Handle String
                     | SActQui
     deriving (Eq,Show)

-- ----------------------------------------------------------------------------------------- --
--  Connections :  connections to outside world


data  ConnHandle    =  ConnHtoW  { chan       :: ChanId
                                 , handle     :: Handle
                                 , vars       :: [VarId]         -- encoding domain
                                 , vexpr      :: VExpr           -- encoding range of String
                                 }
                     | ConnHfroW { chan       :: ChanId
                                 , handle     :: Handle
                                 , var        :: VarId           -- decoding domain of String
                                 , vexprs     :: [VExpr]         -- decoding range
                                 }
     deriving (Eq,Show)


-- ----------------------------------------------------------------------------------------- --
-- data Verdict

data  Verdict  =  Pass
                | Fail Action
                | NoVerdict
     deriving (Eq,Ord,Read,Show)


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

