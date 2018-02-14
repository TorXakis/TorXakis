{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE ViewPatterns #-}
module SNext

-- ----------------------------------------------------------------------------------------- --
--
-- Making Next Step in Behaviour Tree
--
-- ----------------------------------------------------------------------------------------- --
-- export

( updateNode    --  updateNode :: Map.Map FuncId (FuncDef v) -> IWals -> SNode -> SNode
                --  updateNode fdefs iwals node
                --  instantiates symbolic vars in node by iwals
)

where

import qualified Data.Map  as Map

import STree
import FuncDef
import FuncId
import ValExpr
import TreeVars
import TxsDefs
import Variable

type AssignEnv = VarEnv IVar IVar

{-
SHOULD BE AFTER IN SBEHAVE
newPath :: 
    Map.Map FuncId (FuncDef v) -- function definitions
    -> IWals                   -- values for IVars to substitute
    -> [STree]                 -- current determinized state of STree
    -> [STree]                 -- new determinized STree
newPath fdefs iwals 
-}

-- ----------------------------------------------------------------------------------------- --
-- update node :  substitute symbolic IVars in an SNode by a concrete value 

updateNode :: Variable v =>
    Map.Map FuncId (FuncDef v) -- function definitions
    -> IWals                   -- values for IVars to substitute
    -> SNode                   -- Snode in which the IVars will be substituted
    -> SNode
updateNode fdefs iwals = updateNode' fdefs (Map.map cstrConst iwals)



updateNode' :: Variable v => Map.Map FuncId (FuncDef v) -> AssignEnv -> SNode -> SNode

updateNode' fdefs ass (SNbexpr ve bexp)
  = SNbexpr (Map.map (subst ass fdefs) ve) bexp

updateNode' fdefs ass (SNguard cond snode)
  =  SNguard (subst ass fdefs cond) snode

updateNode' fdefs ass (SNchoice snodes)
  =  SNchoice (updateNode' fdefs ass <$> snodes)

updateNode' fdefs ass (SNparallel chids snodes)
  =  SNparallel chids (updateNode' fdefs ass <$> snodes)

updateNode' fdefs ass (SNenable snode1 choffs snode2)
  =  SNenable (updateNode' fdefs ass snode1) choffs (updateNode' fdefs ass snode2)

updateNode' fdefs ass (SNdisable snode1 snode2)
  =  SNdisable (updateNode' fdefs ass snode1) (updateNode' fdefs ass snode2)

updateNode' fdefs ass (SNinterrupt snode1 snode2)
  =  SNinterrupt (updateNode' fdefs ass snode1) (updateNode' fdefs ass snode2)

updateNode' fdefs ass (SNhide chids snode)
  =  SNhide chids (updateNode' fdefs ass snode)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
