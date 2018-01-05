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

import           Network.TextViaSockets (Connection)

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

data  SAction       =  SAct     Connection Text
                     | SActQui
     deriving (Eq,Show)

-- ----------------------------------------------------------------------------------------- --

instance PShow SAction
  where
    pshow (SAct _h s) =  "SAct  "++" ! "++ show s ++"\n"
    pshow  SActQui    =  "No Output (Sut is Quiescent)\n"

-- ----------------------------------------------------------------------------------------- --
--  Connections :  connections to outside world


data  ConnHandle    =  ConnHtoW  { chan       :: ChanId
                                 , connection :: Connection      -- ^ Connection to world
                                 , vars       :: [VarId]         -- ^ Encoding domain
                                 , vexpr      :: VExpr           -- ^ Encoding range of String
                                 }
                     | ConnHfroW { chan       :: ChanId
                                 , connection :: Connection      -- ^ Connection from world
                                 , var        :: VarId           -- ^ Decoding domain of String
                                 , vexprs     :: [VExpr]         -- ^ Decoding range
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
