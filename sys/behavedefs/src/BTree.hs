{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
-- ----------------------------------------------------------------------------------------- --

module BTree

-- ----------------------------------------------------------------------------------------- --
--
-- Interaction Variables, Communication Tree, Behaviour Tree, Behaviour Node
--
-- ----------------------------------------------------------------------------------------- --
-- export

( BTree
, BBranch (..)
, CTree
, CTBranch (..)
, BNode (..)
, CNode
, INode
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Set    as Set

import           TreeVars
import           TxsDefs
import           ValExpr
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --
-- BehAct :  behaviour Action

-- ----------------------------------------------------------------------------------------- --
-- CTree :  communication tree over interaction variables
--          closed, ie. no free variables

type  CTree     =   [ CTBranch ]

data  CTBranch  =  CTpref { ctoffers  :: Set.Set CTOffer              -- set may be empty
                          , cthidvars :: [IVar]                       -- hidden variables
                          , ctpred    :: ValExpr IVar
                          , ctnext    :: INode
                          }
     deriving (Eq,Ord,Read,Show)


-- ----------------------------------------------------------------------------------------- --
-- BTree   :  behaviour tree, ie. communication tree with explicit internal steps
--            BTree is the basic behaviour state structure
--            a BTree is closed, ie. no free variables


type  BTree    =  [ BBranch ]


data  BBranch  =  BTpref   { btoffers  :: Set.Set CTOffer        -- set must be non-empty
                           , bthidvars :: [IVar]                 -- hidden variables
                           , btpred    :: ValExpr IVar
                           , btnext    :: INode
                           }
                | BTtau    { btree        :: BTree
                           }
     deriving (Eq,Ord,Read,Show)


-- ----------------------------------------------------------------------------------------- --
-- BNode :  general behaviour node
-- CNode :  concrete, closed behaviour node, ie. without interaction variables
-- INode :  interaction behaviour node, ie. with interaction variable environment


data  BNode env =  BNbexpr      env BExpr                         -- env must be: (WEnv,IVEnv)
                 | BNparallel   [ChanId] [BNode env]
                 | BNenable     (BNode env) [ChanOffer] (BNode env)
                 | BNdisable    (BNode env) (BNode env)
                 | BNinterrupt  (BNode env) (BNode env)
                 | BNhide       [ChanId] (BNode env)
     deriving (Eq,Ord,Read,Show,Functor)


type CNode   =  BNode (WEnv VarId)                             --  Concrete Behaviour Node

type INode   =  BNode (WEnv VarId, IVEnv)                      --  Interactions Behaviour Node


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
