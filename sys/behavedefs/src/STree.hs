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

( STree (..)
, STtrans (..)
, ActionTrace
, DPath
, SNode (..)
, Det
, treeToPath
, detToPath
, determinize
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Set    as Set

import           TreeVars
import           TxsDefs
import           ValExpr


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

-- List of observable actions
type ActionTrace = [BehAction]

type Det = [STree]

type DPath = [Det] -- determinization of states after every step

-- determinization of a single state
determinize :: STree -> Det
determinize tree = [tree]

-- takes a determinization of states as a starting point for a path
detToPath :: Det -> DPath
detToPath det = [det]

-- takes a state as a starting point for a path
treeToPath :: STree -> DPath
treeToPath = detToPath . determinize


data SNode = SNbexpr      IVEnv BExpr
           | SNchoice     [SNode]
           | SNguard      (ValExpr IVar) SNode
           | SNparallel   [ChanId] [SNode]
           | SNenable     SNode [ChanOffer] SNode
           | SNdisable    SNode SNode
           | SNinterrupt  SNode SNode
           | SNhide       [ChanId] SNode
    deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
