{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module Unfold

-- ----------------------------------------------------------------------------------------- --
-- 
-- Unfold of Behaviour States for the Primer, using Expand
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( unfold   -- :: [ Set.Set TxsDefs.ChanId ] -> CNode -> IOB.IOB BTree
           -- 'unfold chsets bnode' unfolds 'bnode'
           -- (closed, no symbolic or interaction variables)
           -- into a reduced btree (closed, with interaction variables),
           -- plain unfold, no input/ouput, filtered on specification's channelsets
)

-- ----------------------------------------------------------------------------------------- --
-- import 

where

import Control.Monad.State

import qualified Data.Set   as Set
import qualified Data.Map   as Map

-- import from local
import Expand
import Next
import Reduce

import qualified EnvBTree   as IOB
import qualified EnvData

import qualified TxsDefs

import qualified SolveDefs
import qualified Solve

import BTree
import qualified Eval


-- ----------------------------------------------------------------------------------------- --
-- unfold :  unfold CNode ( = BNode () )  into BTree while reducing on the way


unfold :: [ Set.Set TxsDefs.ChanId ] -> CNode -> IOB.IOB BTree
unfold chsets cnode  =  do
     ctree    <- expand chsets cnode
     btree'   <- unfoldCT chsets ctree
     btree''  <- filterBT chsets btree'
     btree''' <- reduce btree''
     return $ btree'''


-- ----------------------------------------------------------------------------------------- --
-- unfoldCT :  transform a communication tree CTree  (without separate tau-branch)
--             recursively, into a behaviour tree BTree (with separate tau-brach)


unfoldCT :: [ Set.Set TxsDefs.ChanId ] -> CTree -> IOB.IOB BTree
unfoldCT chsets ctree  =  do
     btrees <- mapM (unfoldCTbranch chsets) ctree
     return $ concat btrees


unfoldCTbranch :: [ Set.Set TxsDefs.ChanId ] -> CTBranch -> IOB.IOB BTree
unfoldCTbranch chsets (CTpref ctoffs cthidvars' ctpreds ctnext)
   | Set.null ctoffs && null cthidvars'  =  do                -- tau action or nothing
       predVal <- Eval.evalCnrs ctpreds
       if  predVal
         then do nextcnode <- return $ nextNode Map.empty ctnext
                 nextctree <- expand chsets nextcnode
                 nextbtree <- unfoldCT chsets nextctree
                 return $ [ BTtau nextbtree ]
         else do return $ []
   | Set.null ctoffs && not (null cthidvars') =              -- tau action or nothing
       let assertions = foldr Solve.add Solve.empty ctpreds
        in do
           smtEnv <- IOB.getSMT "current"
           (sp,smtEnv') <- lift $ runStateT (Solve.uniSolve cthidvars' assertions) smtEnv
           IOB.putSMT "current" smtEnv'
           case sp of
           { SolveDefs.Solved sol    -> do nextcnode <- return $ nextNode sol ctnext
                                           nextctree <- expand chsets nextcnode
                                           nextbtree <- unfoldCT chsets nextctree
                                           return $ [ BTtau nextbtree ]
           ; SolveDefs.Unsolvable    -> do return $ []
           ; SolveDefs.UnableToSolve -> do IOB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                                         $ "unfoldCTbranch: Not unique" ]
                                           return $ [ BTpref ctoffs cthidvars' ctpreds ctnext ]
           }
   | True =  -- not $ Set.null ctoffs                                  -- visible action or nothing
       let assertions = foldr Solve.add Solve.empty ctpreds
        in do
           vvars  <- return $ concatMap ctchoffers (Set.toList ctoffs)
           smtEnv <- IOB.getSMT "current"
           (sat,smtEnv') <- lift $ runStateT (Solve.satSolve (vvars++cthidvars') assertions)
                                             smtEnv
           IOB.putSMT "current" smtEnv'
           case sat of
           { SolveDefs.Sat     -> do return $ [ BTpref ctoffs cthidvars' ctpreds ctnext ]
           ; SolveDefs.Unsat   -> do return $ []
           ; SolveDefs.Unknown -> do IOB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                                   $ "unfoldCTbranch: Solve Unknown" ]
                                     return $ [ BTpref ctoffs cthidvars' ctpreds ctnext ]
           }

-- ----------------------------------------------------------------------------------------- --
-- filterBT :  filter behaviour tree BTree on visible action-sets from Specification


filterBT :: [ Set.Set TxsDefs.ChanId ] -> BTree -> IOB.IOB BTree
filterBT chsets btree'  =  do
     btrees <- mapM (filterBTbranch chsets) btree'
     return $ concat btrees


filterBTbranch :: [ Set.Set TxsDefs.ChanId ] -> BBranch -> IOB.IOB BTree

filterBTbranch chsets btpref@(BTpref btoffs _ _ _)  =  do
     if (Set.map ctchan btoffs) `elem` chsets
       then return $ [ btpref ]
       else return $ []

filterBTbranch chsets (BTtau btree)  =  do
     btree' <- filterBT chsets btree
     return $ [ BTtau btree' ]
     

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
