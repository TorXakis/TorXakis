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

instance PShow Action
  where
    pshow (Act    set)  =  "Act    { "++ pshow set ++" }\n"
    pshow  ActQui       =  "No Output (Quiescence)\n"

-- ----------------------------------------------------------------------------------------- --
-- SAction :  string encoded actions


data  SAction       =  SAct     Handle String
                     | SActQui
     deriving (Eq,Show)

instance PShow SAction
  where
    pshow (SAct _h s)  =  "SAct  "++" ! "++ show s ++"\n"
    pshow  SActQui    =  "No Output (Sut is Quiescent)\n"
    
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


instance PShow ConnHandle
  where
    pshow (ConnHtoW chan h vars vexp)
      =  pshow chan ++ show h ++ "\n" ++ pshow vars ++ pshow vexp ++ "\n"
    pshow (ConnHfroW chan h var vexps)
      =  pshow chan ++ show h ++ "\n" ++ pshow var ++ pshow vexps ++ "\n"
-- ----------------------------------------------------------------------------------------- --
-- data Verdict

data  Verdict  =  Pass
                | Fail Action
     deriving (Eq,Ord,Read,Show)

instance PShow Verdict
  where
     pshow  Pass       =  "PASS"
     pshow (Fail act)  =  "FAIL:  " ++ fshow act

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

