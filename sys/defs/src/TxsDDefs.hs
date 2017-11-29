{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module TxsDDefs

where

import qualified Data.Set  as Set
import           Data.Text (Text)
import           System.IO

import           ConstDefs (Const)
import           TxsDefs   (ChanId, VExpr)
import           TxsShow
import           VarId     (VarId)

-- ----------------------------------------------------------------------------------------- --
-- Const  :  values (for now: VExpr)
-- Action :  trie/primer valued actions

data  Action   =  Act     ( Set.Set (ChanId,[Const]) )
                | ActQui
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --

instance PShow Action
  where
    pshow (Act    set) =  "Act    { "++ pshow set ++" }\n"
    pshow  ActQui      =  "No Output (Quiescence)\n"

-- ----------------------------------------------------------------------------------------- --
-- SAction :  string encoded actions

data  SAction       =  SAct     Handle Text
                     | SActQui
     deriving (Eq,Show)

-- ----------------------------------------------------------------------------------------- --

instance PShow SAction
  where
    pshow (SAct _h s) =  "SAct  "++" ! "++ show s ++"\n"
    pshow  SActQui    =  "No Output (Sut is Quiescent)\n"

-- ----------------------------------------------------------------------------------------- --
--  Connections :  connections to outside world


data  ConnHandle    =  ConnHtoW  { chan   :: ChanId
                                 , handle :: Handle
                                 , vars   :: [VarId]         -- encoding domain
                                 , vexpr  :: VExpr           -- encoding range of String
                                 }
                     | ConnHfroW { chan   :: ChanId
                                 , handle :: Handle
                                 , var    :: VarId           -- decoding domain of String
                                 , vexprs :: [VExpr]         -- decoding range
                                 }
     deriving (Eq,Show)

-- ----------------------------------------------------------------------------------------- --


instance PShow ConnHandle
  where
    pshow (ConnHtoW c h vs v)
      =  pshow c ++ show h ++ "\n" ++ pshow vs ++ pshow v ++ "\n"
    pshow (ConnHfroW c h v vs)
      =  pshow c ++ show h ++ "\n" ++ pshow v ++ pshow vs ++ "\n"

-- ----------------------------------------------------------------------------------------- --
-- data Verdict

data  Verdict  =  Pass
                | Fail Action
                | NoVerdict
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
instance PShow Verdict
  where
     pshow  Pass      =  "PASS"
     pshow (Fail act) =  "FAIL:  " ++ fshow act
     pshow  NoVerdict =  "No Verdict"

-- ----------------------------------------------------------------------------------------- --
-- test purposes verdict

data PurpVerdict = PurpHit | PurpMiss | PurpHalted
     deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
instance PShow PurpVerdict
  where
     pshow  PurpHit    =  "Hit"
     pshow  PurpMiss   =  "Miss"
     pshow  PurpHalted =  "Halted"

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
