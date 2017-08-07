{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveFunctor #-}
-- ----------------------------------------------------------------------------------------- --

module BTree

-- ----------------------------------------------------------------------------------------- --
--
-- Interaction Variables, Communication Tree, Behaviour Tree, Behaviour Node
--
-- ----------------------------------------------------------------------------------------- --
-- export

( BehAction
, BTree
, BBranch (..)
, CTree
, CTBranch (..)
, CTOffer (..)
, BNode (..)
, CNode
, INode
, IVar (..)
, IWals
, Menu
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

-- import qualified Data.Char as Char
-- import qualified Data.List as List
import qualified Data.Set  as Set
-- import qualified Data.Map  as Map
-- import qualified Data.String.Utils as Utils

-- import from defs
import TxsDefs

-- import SolveDefs
-- import qualified SMTData as SMTData


-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --
-- BehAct :  behaviour Action


type BehAction  =  Set.Set (TxsDefs.ChanId,[TxsDefs.Const])


-- ----------------------------------------------------------------------------------------- --
-- IVar     :  interaction variable for behaviour tree


data  IVar      =  IVar    { ivname     :: Name       -- name of Channel
                           , ivuid      :: Int        -- uid of Channel
                           , ivpos      :: Int        -- 1..length (chansorts chan)
                           , ivstat     :: Int        -- depth in the behaviour tree
                           , ivsrt      :: SortId     -- (chansorts chan)!!(pos-1)
                           }
     deriving (Eq,Ord,Read,Show)


instance Variable IVar
  where
    vname (IVar nm uid pos stat _srt)  =  "$"++nm++ "$"++(show uid)++
                                         "$"++(show stat)++"$"++(show pos)++"$"
    vunid (IVar { ivuid = uid })  =  uid
    vsort (IVar { ivsrt = srt })  =  srt
    cstrVariable s i t = IVar s i (-1) (-1) t           -- PvdL for temporary variable


type  IVEnv     =  VarEnv VarId IVar

type  IWals     =  WEnv IVar


-- ----------------------------------------------------------------------------------------- --
-- CTree :  communication tree over interaction variables
--          closed, ie. no free variables


type  CTree     =  [ CTBranch ]


data  CTBranch  =  CTpref   { ctoffers    :: Set.Set CTOffer              -- set may be empty
                            , cthidvars   :: [IVar]                       -- hidden variables 
                            , ctpreds     :: [ValExpr IVar]
                            , ctnext      :: INode
                            }
     deriving (Eq,Ord,Read,Show)
 

data  CTOffer   =  CToffer  { ctchan      :: ChanId
                            , ctchoffers  :: [IVar]
                            }
     deriving (Eq,Ord,Read,Show)


-- ----------------------------------------------------------------------------------------- --
-- BTree   :  behaviour tree, ie. communication tree with explicit internal steps
--            BTree is the basic behaviour state structure
--            a BTree is closed, ie. no free variables
     

type  BTree    =  [ BBranch ]


data  BBranch  =  BTpref   { btoffers     :: Set.Set CTOffer        -- set must be non-empty
                           , bthidvars    :: [IVar]                 -- hidden variables 
                           , btpreds      :: [ValExpr IVar]
                           , btnext       :: INode
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
-- menu

type  Menu  =  [ ( Set.Set BTree.CTOffer, [BTree.IVar], [TxsDefs.ValExpr BTree.IVar] ) ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
