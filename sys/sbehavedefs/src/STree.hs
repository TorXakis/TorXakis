{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
-- ----------------------------------------------------------------------------------------- --

module STree

-- ----------------------------------------------------------------------------------------- --
--
-- Interaction Variables, Communication Tree, Behaviour Tree, Behaviour Node
--
-- ----------------------------------------------------------------------------------------- --
-- export

( BehAction
--, BTree
--, BBranch (..)
--, CTree
--, CTBranch (..)
, STree (..)
, STtrans (..)
, ActionTrace
, DPath
, CTOffer (..)
, BNode (..)
, CNode
, SNode (..)
, INode
, IVar (..)
, IWals
, Menu
, treeToPath
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Data.Monoid
import qualified Data.Set    as Set
import qualified Data.Text   as T

import           ConstDefs
import           Id
import           Name
import           SortId
--import           TreeMonad
import           TxsDefs
import           ValExpr
import           Variable
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --
-- BehAct :  behaviour Action


type BehAction  =  Set.Set (TxsDefs.ChanId,[Const])


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
                           , ivuid  :: Id         -- uid of Channel
                           , ivpos  :: Int        -- 1..length (chansorts chan)
                           , ivstat :: Int        -- depth in the behaviour tree
                           , ivsrt  :: SortId     -- (chansorts chan)!!(pos-1)
                           }
     deriving (Eq,Ord,Read,Show)


instance Variable IVar where
    vname (IVar nm uid pos stat _srt) =
      "$" <> nm <> "$" <> (T.pack . show) uid <> "$" <> (T.pack . show) stat <> "$" <> (T.pack . show) pos <> "$"
    vunid IVar{ ivuid = uid } = _id uid
    vsort IVar{ ivsrt = srt } = srt
    cstrVariable s i = IVar (T.pack s) (Id i) (-1) (-1)           -- PvdL for temporary variable

type  IVEnv = VarEnv VarId IVar

type  IWals = WEnv IVar


-- ----------------------------------------------------------------------------------------- --
-- STree :  communication tree over interaction variables
--          may contain free variables


-- unfolded STS, nodes are bexprs and edges are symbolic
data STree = STree 
    { stnode  :: SNode
    , sttrans :: [STtrans]
    }

instance Eq STree where
    (==) STree{stnode = node1} STree{stnode = node2} = node1 == node2

instance Show STree where
    show stree = show $ stnode stree

instance Ord STree where
    compare STree{stnode = node1} STree{stnode = node2} = compare node1 node2


data STtrans
    = STtrans
    { stoffers  :: Set.Set CTOffer  -- set may be empty, representing tau-transition
    , sthidvars :: [IVar]           -- hidden variables
    , stpred    :: ValExpr IVar     -- predicates
    , stnext    :: STree
    }
--    | STtau
--    { sthidvars :: [IVar]
--    , stpred    :: ValExpr IVar
--    , stnext    :: STree
--    }
    deriving (Eq,Ord,Show)

data  CTOffer   =  CToffer  { ctchan     :: ChanId
                            , ctchoffers :: [IVar]
                            }
     deriving (Eq,Ord,Read,Show)

-- List of observable actions
type ActionTrace = [BehAction]

type DPath = [[STree]] -- determinization of states after every step

-- takes a tree as a starting point for a path
treeToPath :: STree -> DPath
treeToPath tree = [[tree]]

{-
-- if non-deterministically in the set of states, and every of those states
-- should take any of the branches in its list, this function picks a
-- concrete value, by symbolic execution
solve :: (Set.Set [STBranch]) -> IOC.IOC DTree
solve ndState =
    let partitions = partition <$> ndState
    in ...
    where
    -- partition transitions of every 
    partitions :: [STBranch] -> Map.Map (Set.Set CTOffer) [STBranch]
    partitions branches = foldr addToPartition Map.empty branches
    addToPartition :: STBranch
                      -> Map.Map (Set.Set CTOffer) [STBranch]
                      -> Map.Map (Set.Set CTOffer) [STBranch]
    addToPartition branch{stoffers=offs} map = insertWith (++) offs [branch]
-}

-- ----------------------------------------------------------------------------------------- --
-- CTree :  communication tree over interaction variables
--          closed, ie. no free variables
{-
type  CTree     =   [ CTBranch ]

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
-}

-- ----------------------------------------------------------------------------------------- --
-- BNode :  general behaviour node
-- CNode :  concrete, closed behaviour node, ie. without interaction variables
-- INode :  interaction behaviour node, ie. with interaction variable environment

--data SEnvNode = SNexpr IVEnv BExpr

data SNode = SNbexpr      IVEnv BExpr
           | SNchoice     [SNode]
           | SNguard      (ValExpr IVar) SNode
           | SNparallel   [ChanId] [SNode]
           | SNenable     SNode [ChanOffer] SNode
           | SNdisable    SNode SNode
           | SNinterrupt  SNode SNode
           | SNhide       [ChanId] SNode
    deriving (Eq,Ord,Read,Show)

data  BNode env =  BNbexpr      env BExpr                         -- env must be: (WEnv,IVEnv)
                 | BNparallel   [ChanId] [BNode env]
                 | BNenable     (BNode env) [ChanOffer] (BNode env)
                 | BNdisable    (BNode env) (BNode env)
                 | BNinterrupt  (BNode env) (BNode env)
                 | BNhide       [ChanId] (BNode env)
     deriving (Eq,Ord,Read,Show,Functor)

--data SNode   =  BNode IVEnv

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
type  Menu  =  [ ( Set.Set STree.CTOffer, [STree.IVar], ValExpr STree.IVar ) ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
