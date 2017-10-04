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

import           Data.Monoid
import qualified Data.Set    as Set
import qualified Data.Text   as T

-- import from defs
import           TxsDefs


-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --
-- BehAct :  behaviour Action


type BehAction  =  Set.Set (TxsDefs.ChanId,[TxsDefs.Const])


-- | IVar     :  interaction variable for behaviour tree
--
-- An interaction variable is used to combine the communication of different
-- processes over the same channel.
--
-- The following channel communication:
--
-- > A ? x ? y ? z
--
-- Is associated to the following `IVar`'s:
--
-- > IVar "A" uid 1 d sortOf(x)
-- > IVar "A" uid 2 d sortOf(y)
-- > IVar "A" uid 3 d sortOf(z)
--
-- These variables allow to translate communications like:
--
-- > A ! 6
--
-- which gets translated to:
--
-- > A ? A1 [[ A1 == 6 ]]
--
-- where A1 is associated to `IVar`:
--
-- > IVar "A" uid 1 d Int
--
data  IVar      =  IVar    { ivname :: Name       -- name of Channel
                           , ivuid  :: Int        -- uid of Channel
                           , ivpos  :: Int        -- 1..length (chansorts chan)
                           , ivstat :: Int        -- depth in the behaviour tree
                           , ivsrt  :: SortId     -- (chansorts chan)!!(pos-1)
                           }
     deriving (Eq,Ord,Read,Show)


instance Variable IVar where
    vname (IVar nm uid pos stat _srt) =
      "$" <> nm <> "$" <> (T.pack . show) uid <> "$" <> (T.pack . show) stat <> "$" <> (T.pack . show) pos <> "$"
    vunid IVar{ ivuid = uid } = uid
    vsort IVar{ ivsrt = srt } = srt
    cstrVariable s i = IVar (T.pack s) i (-1) (-1)           -- PvdL for temporary variable

type  IVEnv = VarEnv VarId IVar

type  IWals = WEnv IVar

-- ----------------------------------------------------------------------------------------- --
-- CTree :  communication tree over interaction variables
--          closed, ie. no free variables

type  CTree     =  [ CTBranch ]

data  CTBranch  =  CTpref { ctoffers  :: Set.Set CTOffer              -- set may be empty
                          , cthidvars :: [IVar]                       -- hidden variables
                          , ctpred    :: ValExpr IVar
                          , ctnext    :: INode
                          }
     deriving (Eq,Ord,Read,Show)

data  CTOffer   =  CToffer  { ctchan     :: ChanId
                            , ctchoffers :: [IVar]
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
-- menu

-- |
--
-- An element of the form (offers, hiddenVars, valexp)
--
-- Example offers G ? x | H ? y ~ Set { G ? x ,  H ? y }
--
-- valexp: = value expression over interaction variables. interaction variables
-- must come from the hidden variables or offers.
type  Menu  =  [ ( Set.Set BTree.CTOffer, [BTree.IVar], TxsDefs.ValExpr BTree.IVar ) ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
