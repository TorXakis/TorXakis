{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Behave

-- ----------------------------------------------------------------------------------------- --
-- 
-- Test Primitives over LTS to BTree -- no IO
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( behInit      -- :: [ Set.Set TxsDefs.ChanId ] -> TxsDefs.BExpr -> IOB.IOB (Maybe BTree)
               -- initialize BTree
, behMayMenu   -- :: [ Set.Set TxsDefs.ChanId ] -> BTree -> Menu
               -- may menu of BTree without quiescence
, behMustMenu  -- :: [ Set.Set TxsDefs.ChanId ] -> BTree -> Menu    -- TODO
               -- must menu of BTree without quiescence
, behRefusal   -- :: BTree -> Set.Set TxsDefs.ChanId -> Bool
               -- check refusal set on BTree
, behAfterAct  -- :: [ Set.Set TxsDefs.ChanId ] -> BTree -> BehAction -> IOB.IOB (Maybe BTree)
               -- perform after action on BTree
, behAfterRef  -- :: BTree -> Set.Set TxsDefs.ChanId -> IOB.IOB (Maybe BTree)
               -- perform after refusal on BTree
)

-- ----------------------------------------------------------------------------------------- --
-- import 

where

import Control.Monad.State

import qualified Data.Set  as Set
import qualified Data.Map  as Map

-- import from local
import Next
import Reduce
import Unfold

-- import from behavedef
import BTree

-- import from behaveenv
import qualified EnvBTree  as IOB

-- import from coreenv
import qualified EnvData   as EnvData

-- import from defs
import qualified TxsDefs   as  TxsDefs
import qualified TxsUtils  as  TxsUtils

-- import from solve
import SolveDefs
import Solve

-- import from value
import qualified Eval as Eval


-- ----------------------------------------------------------------------------------------- --
-- behInit :  initialize BTree


behInit :: [ Set.Set TxsDefs.ChanId ] -> TxsDefs.BExpr -> IOB.IOB (Maybe BTree)
behInit chsets bexp  =  do
     btree <- unfold chsets (BNbexpr Map.empty bexp)
     return $ Just btree


-- ----------------------------------------------------------------------------------------- --
-- behMayMenu :  may menu of BTree without quiescence


behMayMenu :: [ Set.Set TxsDefs.ChanId ] -> BTree -> Menu
behMayMenu chsets btree 
  =  [ ( btoffs, hidvars, preds ) | BTpref btoffs hidvars preds next <- btree ]
     ++ concat [ behMayMenu chsets btree' | BTtau btree' <- btree ]


-- ----------------------------------------------------------------------------------------- --
-- behMustMenu :  must menu of BTree without quiescence


behMustMenu :: [ Set.Set TxsDefs.ChanId ] -> BTree -> Menu
behMustMenu chsets btree
  =  []

--   TODO

{-   case [ btree' | BTtau btree' <- btree ] of
     { []      -> [ ( btoffs, hidvars, preds ) | BTpref btoffs hidvars preds next <- btree ]
     ; btrees' -> concat $ map behMustMenu btrees'
-}


-- ----------------------------------------------------------------------------------------- --
-- behRefusal :  check refusal set on BTree


behRefusal :: BTree -> Set.Set TxsDefs.ChanId -> Bool
behRefusal btree refset 
  =  case [ btree' | BTtau btree' <- btree ] of
     { []      -> and [ refBBranch bbranch refset | bbranch <- btree ]
     ; btrees' -> or $ map (\bt -> behRefusal bt refset) btrees'
     }


refBBranch :: BBranch -> Set.Set TxsDefs.ChanId -> Bool

refBBranch (BTpref btoffs hidvars preds next) refset
  =  not $ (Set.map ctchan btoffs) `Set.isSubsetOf` refset

refBBranch (BTtau btree) refset 
  =  False


-- ----------------------------------------------------------------------------------------- --
-- behAfterAct :  perform after action on BTree


behAfterAct :: [ Set.Set TxsDefs.ChanId ] -> BTree -> BehAction -> IOB.IOB (Maybe BTree)
behAfterAct chsets btree behact
 | Set.null behact  =  do
     IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "behAfterAct: after empty set/tau action" ]
     return $ Nothing
 | not $ Set.null behact  =  do
     afters <- afterActBTree chsets behact btree
     if  null afters
       then do return $ Nothing
       else do newbtree  <- return $ map BTtau afters
               newbtree' <- reduce $ newbtree
               return $ Just newbtree'


-- ----------------------------------------------------------------------------------------- --
-- afterActBTree :  list of possible BTree states of btree after non-empty behact
--               :  result list is empty: behact is not a possible action
--               :  result contains empty list: STOP is possible after state


afterActBTree :: [ Set.Set TxsDefs.ChanId ] -> BehAction -> BTree -> IOB.IOB [BTree]
afterActBTree chsets behact btree  =  do
     newbtrees <- mapM (afterActBBranch chsets behact) btree
     return $ concat newbtrees


-- ----------------------------------------------------------------------------------------- --
-- afterBBranch  :  list of possible BTree states of BBranch after non-empty behact
--               :  result list is empty: behact is not a possible action
--               :  result contains empty list: STOP is possible after state


afterActBBranch :: [ Set.Set TxsDefs.ChanId ] -> BehAction -> BBranch -> IOB.IOB [BTree]

afterActBBranch chsets behact (BTpref btoffs [] preds next)  =  do
     match <- matchAct2CTOffer behact btoffs
     case match of
       Nothing    -> do return $ []
       Just iwals -> let preds' = map (TxsUtils.partSubst (Map.map TxsDefs.cstrConst iwals)) preds
                      in do condition <- Eval.evalCnrs preds'
                            if  condition
                              then do cnode <- return $ nextNode iwals next
                                      after <- unfold chsets cnode
                                      return $ [after]
                              else do return $ []

afterActBBranch chsets behact (BTpref btoffs hidvars preds next)  =  do
     match <- matchAct2CTOffer behact btoffs
     case match of
       Nothing
        -> do return $ []
       Just iwals
        -> do preds' <- return $ map (TxsUtils.partSubst (Map.map TxsDefs.cstrConst iwals)) preds
              let assertions = foldr add empty preds'
               in do smtEnv <- IOB.getSMT "current"
                     (sat,smtEnv') <- lift $ runStateT (uniSolve hidvars assertions) smtEnv
                     IOB.putSMT "current" smtEnv'
                     case sat of
                       Unsolvable -> do return []
                       Solved sol -> do cnode <- return $ nextNode (iwals `Map.union` sol) next
                                        after <- unfold chsets cnode
                                        return $ [after]
                       UnableToSolve -> do IOB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                                         $ "after: hidden variables not unique" ]
                                           return []

afterActBBranch chsets behact (BTtau btree)  =  do
     afterActBTree chsets behact btree


-- ----------------------------------------------------------------------------------------- --
-- matchAct2CTOffer     :  match a set of CTOffers with values of an action
--   result = Nothing   :  no instantiation possible
--   result = Just map  :  matching map; can be empty: only gates, no values


matchAct2CTOffer :: BehAction -> Set.Set CTOffer -> IOB.IOB (Maybe IWals)
matchAct2CTOffer behact ctoffs  =  do
     if  Set.null ctoffs
       then do
         IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "matchAct2CTOffer: empty ctoffs" ]
         return $ Nothing
       else
         if  Set.map fst behact == Set.map ctchan ctoffs
           then return $ Just $ Map.fromList
                              $ concat [ zip ctchoffs wals
                                       | CToffer chid1 ctchoffs <- Set.toList ctoffs
                                       , ( chid2, wals )        <- Set.toList behact
                                       , chid1 == chid2
                                       ]
           else return $ Nothing


-- ----------------------------------------------------------------------------------------- --
-- behAfterRef :  perform after refusal on BTree


behAfterRef :: BTree -> Set.Set TxsDefs.ChanId -> IOB.IOB (Maybe BTree)
behAfterRef btree refset  =  do
     afters <- afterRefBTree refset btree
     if  null afters
       then do return $ Nothing
       else do newbtree  <- return $ map BTtau afters
               newbtree' <- reduce $ newbtree
               return $ Just newbtree'


-- ----------------------------------------------------------------------------------------- --
-- afterRefBTree :  list of possible BTree states after refusal
--               :  result list is empty: refusal is not possible
--               :  result contains empty list: STOP is possible after refusal


afterRefBTree :: Set.Set TxsDefs.ChanId -> BTree -> IOB.IOB [BTree]
afterRefBTree refset btree  =  do
     case [ btree' | BTtau btree' <- btree ] of
     { []      -> do if and [ refBBranch bbranch refset | bbranch <- btree ]
                       then return $ [btree]
                       else return $ []
     ; btrees' -> do btrees'' <- mapM (afterRefBTree refset) btrees'
                     return $ concat btrees''
     }

 
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

