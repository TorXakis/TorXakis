{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module NComp

-- ----------------------------------------------------------------------------------------- --
--
-- Test selection by N-Complete algorithm for ioco
--
-- ----------------------------------------------------------------------------------------- --
-- export

( nComplete   -- :: TxsDefs.ProcDef -> IOC.IOC TxsDefs.PurpDef
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.List   as List
import           Data.Monoid
import qualified Data.Set    as Set
import qualified Data.Text           as T

import qualified EnvCore     as IOC

import qualified StdTDefs
import qualified TxsDefs

-- ----------------------------------------------------------------------------------------- --
-- nComplete

nComplete :: [ Set.Set TxsDefs.ChanId] -> [ Set.Set TxsDefs.ChanId] ->
             TxsDefs.StatId -> [TxsDefs.Trans] ->
             IOC.IOC (Maybe TxsDefs.PurpDef)

nComplete insyncs outsyncs
          ini@(TxsDefs.StatId nm uid (TxsDefs.ProcId nm' uid' _ _ _)) transs =
     let splsyncs = [ Set.singleton StdTDefs.chanId_Qstep
                    , Set.singleton StdTDefs.chanId_Hit
                    , Set.singleton StdTDefs.chanId_Miss
                    ]
         gids     = [ TxsDefs.GoalId ("Goal_" <> nm <> nm' <> (T.pack . show) n ) (uid*uid'+n) | n <- [1..] ]
         goals    = [ (gid,bexp) | (gid,bexp) <- zip gids (allPaths ini transs) ]
      in return $ Just $ TxsDefs.PurpDef insyncs outsyncs splsyncs goals

allPaths :: TxsDefs.StatId -> [TxsDefs.Trans] -> [TxsDefs.BExpr]
allPaths ini transs = [ path2bexpr p
                         | p@(TxsDefs.Trans from _a _u _to : _pp) <- List.permutations transs
                         , isPath p
                         , from == ini
                         ]

isPath :: [TxsDefs.Trans] -> Bool
isPath []                 = True
isPath [TxsDefs.Trans {}] = True
isPath (TxsDefs.Trans _from _a _u to : TxsDefs.Trans from' a' u' to' : pp) =
    to == from' && isPath (TxsDefs.Trans from' a' u' to' : pp)

path2bexpr :: [TxsDefs.Trans] -> TxsDefs.BExpr
path2bexpr [] = TxsDefs.ActionPref
                    (TxsDefs.ActOffer (Set.singleton $ TxsDefs.Offer StdTDefs.chanId_Hit []) (TxsDefs.cstrConst (TxsDefs.Cbool True)))
                    TxsDefs.Stop
path2bexpr (TxsDefs.Trans _from a _u _to : pp) = TxsDefs.ActionPref a (path2bexpr pp)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
