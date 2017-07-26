{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

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

import Control.Monad.State

import qualified Data.Set  as Set
import qualified Data.List as List

import qualified EnvCore   as IOC
import qualified EnvData   as EnvData

import qualified TxsDefs   as TxsDefs
import qualified StdTDefs  as StdTDefs


-- ----------------------------------------------------------------------------------------- --
-- nComplete


nComplete :: [ Set.Set TxsDefs.ChanId] -> [ Set.Set TxsDefs.ChanId] ->
             TxsDefs.StatId -> [TxsDefs.Trans] ->
             IOC.IOC (Maybe TxsDefs.PurpDef)

nComplete insyncs outsyncs
          init@(TxsDefs.StatId nm uid (TxsDefs.ProcId nm' uid' _ _ _)) transs  =  do
     let splsyncs = [ Set.singleton StdTDefs.chanId_Qstep
                    , Set.singleton StdTDefs.chanId_Hit
                    , Set.singleton StdTDefs.chanId_Miss
                    ]
         gids     = [ TxsDefs.GoalId ("Goal_"++nm++nm'++(show n)) (uid*uid'+n) | n <- [1..] ]
         goals    = [ (gid,bexp) | (gid,bexp) <- zip gids (allPaths init transs) ]
      in return $ Just $ TxsDefs.PurpDef insyncs outsyncs splsyncs goals
                              

allPaths :: TxsDefs.StatId -> [TxsDefs.Trans] -> [TxsDefs.BExpr]
allPaths init transs  =  [ path2bexpr p
                         | p@(TxsDefs.Trans from a u to : pp) <- List.permutations transs
                         , isPath p
                         , from == init
                         ]

isPath :: [TxsDefs.Trans] -> Bool
isPath []                                =  True
isPath (TxsDefs.Trans from a u to : [])  =  True
isPath (TxsDefs.Trans from a u to : TxsDefs.Trans from' a' u' to' : pp)
  =  to == from'  && isPath (TxsDefs.Trans from' a' u' to' : pp)

path2bexpr :: [TxsDefs.Trans] -> TxsDefs.BExpr
path2bexpr []  =  TxsDefs.ActionPref
                    (TxsDefs.ActOffer (Set.singleton $ TxsDefs.Offer StdTDefs.chanId_Hit []) [])
                    TxsDefs.Stop
path2bexpr (TxsDefs.Trans from a u to : pp)  =  TxsDefs.ActionPref a (path2bexpr pp)


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

