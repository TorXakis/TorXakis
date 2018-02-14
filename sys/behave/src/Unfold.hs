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

import BTree
import ConstDefs
import qualified EnvBTree   as IOB
import qualified EnvData
import Expand
import Next
import Reduce
import qualified SolveDefs
import qualified Solve
import TreeVars
import qualified TxsDefs
import           ValExpr


-- ----------------------------------------------------------------------------------------- --
-- unfold :  unfold CNode ( = BNode () )  into BTree while reducing on the way


unfold :: [ Set.Set TxsDefs.ChanId ] -> CNode -> IOB.IOB BTree
unfold chsets cnode = do
     ctree    <- expand chsets cnode
     btree'   <- unfoldCT chsets ctree
     btree''  <- filterBT chsets btree'
     reduce btree''

-- ----------------------------------------------------------------------------------------- --
-- unfoldCT :  transform a communication tree CTree  (without separate tau-branch)
--             recursively, into a behaviour tree BTree (with separate tau-brach)


unfoldCT :: [ Set.Set TxsDefs.ChanId ] -> CTree -> IOB.IOB BTree
unfoldCT chsets ctree = do
     btrees <- mapM (unfoldCTbranch chsets) ctree
     return $ concat btrees


unfoldCTbranch :: [ Set.Set TxsDefs.ChanId ] -> CTBranch -> IOB.IOB BTree
unfoldCTbranch chsets (CTpref ctoffs cthidvars' ctpred' ctnext')
   | Set.null ctoffs =                                           -- tau action or nothing
        if null cthidvars'
          then
            case ValExpr.eval ctpred' of
              Right (Cbool True)          -> do let nextcnode = nextNode Map.empty ctnext'
                                                nextctree <- expand chsets nextcnode
                                                nextbtree <- unfoldCT chsets nextctree
                                                return [ BTtau nextbtree ]
              Right (Cbool False)         -> return []
              Right _                     -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                              "unfoldCTbranch - ctpred' is not a Boolean value"]
                                                return []
              Left s                      -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                                              ("unfoldCTbranch - ctpred' is not a value - " ++ show s)]
                                                return []
          else
            let assertion = Solve.add ctpred' Solve.empty
              in do
                smtEnv <- IOB.getSMT "current"
                (sp,smtEnv') <- lift $ runStateT (Solve.uniSolve cthidvars' assertion) smtEnv
                IOB.putSMT "current" smtEnv'
                case sp of
                  SolveDefs.Solved sol    -> do let nextcnode = nextNode sol ctnext'
                                                nextctree <- expand chsets nextcnode
                                                nextbtree <- unfoldCT chsets nextctree
                                                return [ BTtau nextbtree ]
                  SolveDefs.Unsolvable    -> return []
                  SolveDefs.UnableToSolve -> do IOB.putMsgs [ EnvData.TXS_CORE_USER_ERROR "unfoldCTbranch: Not unique" ]
                                                return [ BTpref ctoffs cthidvars' ctpred' ctnext' ]
   | otherwise =                                                  -- visible action or nothing
       let assertion = Solve.add ctpred' Solve.empty
           vvars = concatMap ctchoffers (Set.toList ctoffs)
         in do
           smtEnv <- IOB.getSMT "current"
           (sat,smtEnv') <- lift $ runStateT (Solve.satSolve (vvars++cthidvars') assertion)
                                             smtEnv
           IOB.putSMT "current" smtEnv'
           case sat of
             SolveDefs.Sat     -> return [ BTpref ctoffs cthidvars' ctpred' ctnext' ]
             SolveDefs.Unsat   -> return []
             SolveDefs.Unknown -> do IOB.putMsgs [ EnvData.TXS_CORE_USER_ERROR "unfoldCTbranch: Solve Unknown" ]
                                     return [ BTpref ctoffs cthidvars' ctpred' ctnext' ]

-- ----------------------------------------------------------------------------------------- --
-- filterBT :  filter behaviour tree BTree on visible action-sets from Specification

filterBT :: [ Set.Set TxsDefs.ChanId ] -> BTree -> IOB.IOB BTree
filterBT chsets btree' = do
     btrees <- mapM (filterBTbranch chsets) btree'
     return $ concat btrees

filterBTbranch :: [ Set.Set TxsDefs.ChanId ] -> BBranch -> IOB.IOB BTree

filterBTbranch chsets btpref@(BTpref btoffs _ _ _) =
     if Set.map ctchan btoffs `elem` chsets
       then return [ btpref ]
       else return []

filterBTbranch chsets (BTtau bt) = do
     btree' <- filterBT chsets bt
     return [ BTtau btree' ]

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
