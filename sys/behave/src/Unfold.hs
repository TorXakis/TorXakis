{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Unfold

-- ----------------------------------------------------------------------------------------- --
-- 
-- Unfold of Behaviour States for the Primer, using Expand
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( unfold   --  unfold :: CNode -> IOE BTree
           --  'unfold bnode' unfolds 'bnode' (closed, no symbolic or interaction variables)
           --  into a reduced btree (closed, with interaction variables),
           --  plain unfold, no input/ouput, filtered on specification's channelsets
           --  state changes on IOE: fresh variables
)

-- ----------------------------------------------------------------------------------------- --
-- import 

where

import System.IO
import Control.Monad.State

import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.Char as Char

import TxsDefs

import CTShow
import TxsShow

import SolveDefs
import Solve

import TxsEnv
import CTree
import Eval
import Expand
import Next
import Reduce


-- ----------------------------------------------------------------------------------------- --
-- unfold :  unfold CNode ( = BNode () )  into BTree while reducing on the way


unfold :: CNode -> IOE BTree
unfold cnode  =  do
     ctree   <- expand cnode
     btree   <- unfoldCT ctree
     btree'  <- filterBT btree
     btree'' <- reduce btree'
     return $ btree''


-- ----------------------------------------------------------------------------------------- --
-- unfoldCT :  transform a communication tree CTree  (without separate tau-branch)
--             recursively, into a behaviour tree BTree (with separate tau-brach)


unfoldCT :: CTree -> IOE BTree
unfoldCT ctree  =  do
     btrees <- mapM unfoldCTbranch ctree
     return $ concat btrees


unfoldCTbranch :: CTBranch -> IOE BTree
unfoldCTbranch (CTpref ctoffs cthidvars ctpreds ctnext)
   | (Set.null ctoffs) && (null cthidvars)  =  do                 -- tau action or nothing
       predVal <- evalCnrs ctpreds
       if  predVal
         then do nextcnode <- return $ nextNode Map.empty ctnext
                 nextctree <- expand nextcnode
                 nextbtree <- unfoldCT nextctree
                 return $ [ BTtau nextbtree ]
         else do return $ []
   | (Set.null ctoffs) && (not $ null cthidvars)  =            -- tau action or nothing
       let assertions = foldr add Solve.empty ctpreds in do
           smtEnv <- getSMT "current"
           (sp,smtEnv') <- lift $ runStateT (uniSolve cthidvars assertions) smtEnv
           putSMT "current" smtEnv'
           case sp of
           { Solved sol    -> do nextcnode <- return $ nextNode sol ctnext
                                 nextctree <- expand nextcnode
                                 nextbtree <- unfoldCT nextctree
                                 return $ [ BTtau nextbtree ]
           ; Unsolvable    -> do return $ []
           ; UnableToSolve -> do lift $ hPutStrLn stderr $ "TXS Unfold unfoldCTbranch: Not unique\n"
                                 return $ [ BTpref ctoffs cthidvars ctpreds ctnext ]
           }
   | (not $ Set.null ctoffs)  =                                 -- visible action or nothing
       let assertions = foldr add Solve.empty ctpreds in do
           vvars <- return $ concat $ map ctchoffers (Set.toList ctoffs)
           smtEnv <- getSMT "current"
           (sat,smtEnv') <- lift $ runStateT (satSolve (vvars ++ cthidvars) assertions) smtEnv
           putSMT "current" smtEnv'
           case sat of
           { Sat     -> do return $ [ BTpref ctoffs cthidvars ctpreds ctnext ]
           ; Unsat   -> do return $ []
           ; Unknown -> do lift $ hPutStrLn stderr $ "TXS Unfold unfoldCTbranch: Solve Unknown\n"
                           return $ [ BTpref ctoffs cthidvars ctpreds ctnext ]
           }


-- ----------------------------------------------------------------------------------------- --
-- filterBT :  filter behaviour tree BTree on visible action-sets from Specification


filterBT :: BTree -> IOE BTree
filterBT btree  =  do
     specdef <- getSpec
     case specdef of
     { DefNo                -> do lift $ hPutStrLn stderr $ "No Spec yet\n"
                                  return $ []
     ; DefModel (ModelDef chinsets choutsets _ bexp) 
                            -> do chsets <- return $ chinsets ++ choutsets
                                  btrees <- mapM (filterBTbranch chsets) btree
                                  return $ concat btrees
     }


filterBTbranch :: [ Set.Set ChanId ] -> BBranch -> IOE BTree

filterBTbranch chsets btpref@(BTpref btoffs _ _ _)  =  do
     if (Set.map ctchan btoffs) `elem` chsets
       then do return $ [ btpref ]
       else do return $ []

filterBTbranch chsets (BTtau btree)  =  do
     btree' <- filterBT btree
     return $ [ BTtau btree' ]
     

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
    
