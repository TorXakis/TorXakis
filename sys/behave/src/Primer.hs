{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Primer

-- ----------------------------------------------------------------------------------------- --
-- 
-- Test Primitives for IOCO (testing with inputs and outputs) :  Menu, Quiescence, doAfter 
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( initBTree     --  initBTree :: IOE ()
                --  initialize BTree in State Env
--, initMapper    --  initMapper :: IOE ()
--                --  initialize Mapper
, menu          --  menu :: BTree -> [ ( Set.Set CTOffer, [ValExpr IVar] ) ]
                --  menu of BTree
, menuIn        --  menuIn :: IOE [ ( Set.Set CTOffer, [SExpr] ) ]
                --  current menu of input CTOffers, no quiescence, according to ioco
, menuOut       --  menuOut :: IOE [ ( Set.Set CTOffer, [SExpr] ) ]
                --  current menu of output CTOffers on current BTree, without quiescence
, isQui         --  isQui :: IOE Bool
                --  check quiescence 
, doAfter       --  doAfter :: Action -> IOE Bool
                --  do after 'action' for testing on current btree
                --  and change State Env accordingly;
                --  result gives success, ie. whether act can be done, if not Env is unchanged;
                --  act must be a non-empty action; Act ActIn ActOut or ActQui
, afterBBranch  -- afterBBranch :: (Set.Set (ChanId,[Const])) -> BBranch -> IOE [BTree]
                --  determine resulting BTree(s) after action acts for one BBranch
, isInCTOffers  --  isInCTOffers :: (Set.Set CTOffer) -> IOE Bool
                --  is input offer?
, isOutCTOffers -- isOutCTOffers :: (Set.Set CTOffer) -> IOE Bool
                --  is output offer?
)

-- ----------------------------------------------------------------------------------------- --
-- import 

where

import System.IO
import Control.Monad.State

import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import TxsDDefs
import TxsUtils
import TxsShow

import SolveDefs
import Solve

import TxsEnv
import Utils
import CTree
import CTShow
import Eval
import Unfold
import Next
import Reduce


-- ----------------------------------------------------------------------------------------- --
-- initBTree  :  initialize environment with BTree


initBTree :: IOE ()
initBTree  =  do
     modify ( \env -> env { envinit = 0
                          , envnrst = 1
                          , envcurs = 0
                          , envtrie = []
                          }
            )
     DefModel (ModelDef _ _ _ bexp) <- getSpec
     btree               <- unfold (BNbexpr Map.empty bexp)
     btree'              <- reduce btree
     modify ( \env -> env { envs2bt = Map.singleton 0 btree' } )
 

-- ----------------------------------------------------------------------------------------- --
-- initMapper :  initialize environment with BTree

{-

initMapper :: IOE ()
initMapper =  do
     ModelDef _ _ bexp <- getMapper
     btree             <- unfold (BNbexpr Map.empty bexp)
     btree'            <- reduce btree
     modify ( \env -> env { envmapper = btree' } )
 
-}

-- ----------------------------------------------------------------------------------------- --
-- menu     :  basic menu of CTOffers on argument btree, no quiescence
-- menuIn   :  menu of input CTOffers on current BTree, no quiescence, according to ioco
-- menuOut  :  menu of output CTOffers on current BTree, no quiescence


menu :: BTree -> [ ( Set.Set CTOffer, [IVar], [ValExpr IVar] ) ]
menu btree
  =  [ ( btoffs, hidvars, preds ) | BTpref btoffs hidvars preds next <- btree ]
     ++ concat [ menu btree' | BTtau btree' <- btree ]


menuIn :: IOE [ ( Set.Set CTOffer, [IVar], [ValExpr IVar] ) ]
menuIn  =  do
     curbtree <- getBTree
     filterM (isInCTOffers.frst) (menu curbtree)


menuOut :: IOE [ ( Set.Set CTOffer, [IVar], [ValExpr IVar] ) ]
menuOut  =  do
     curbtree <- getBTree
     filterM (isOutCTOffers.frst) (menu curbtree)


-- ----------------------------------------------------------------------------------------- --
-- isQui  :  test for quiescence on current BTree


isQui :: IOE Bool
isQui  =  do
     curbtree <- getBTree
     afters   <- afterQui curbtree
     return $ not ( null afters )


-- ----------------------------------------------------------------------------------------- --
-- doAfter :  do action 'act' on current btree and change environment accordingly
-- result gives success, ie. whether act can be done, if not succesful Env is not changed
-- act must be a non-empty action; Act ActIn ActOut or ActQui


doAfter :: Action -> IOE Bool

doAfter (Act acts)
 | Set.null acts        =  do
     lift $ hPutStrLn stderr $ "TXS Primer doAfter: empty set/tau action\n"
     return $ False
 | not $ Set.null acts  =  do
     curbtree <- getBTree
     afters   <- afterBTree acts curbtree
     if  null afters
       then do
         return False
       else do
         newbtree  <- return $ map BTtau afters
         newbtree' <- reduce newbtree
         modify ( \env -> env { envnrst = (envnrst env) + 1
                              , envcurs = (envnrst env)
                              , envtrie = (envcurs env, Act acts, envnrst env) : (envtrie env)
                              , envs2bt = Map.insert (envnrst env) newbtree' (envs2bt env)
                              }
                )
         return True

doAfter (ActQui)  =  do
     curbtree <- getBTree
     afters   <- afterQui curbtree
     if  null afters
       then do
         return False
       else do
         newbtree  <- return $ map BTtau afters
         newbtree' <- reduce newbtree
         modify ( \env -> env { envnrst = (envnrst env) + 1
                              , envcurs = (envnrst env)
                              , envtrie = (envcurs env, ActQui, envnrst env) : (envtrie env)
                              , envs2bt = Map.insert (envnrst env) newbtree' (envs2bt env)
                              }
                )
         return True


-- ----------------------------------------------------------------------------------------- --
-- afterBTree    :  possible BTree states of BTree after acts
-- afterBBranch  :  possible BTree states of BBranch after 'acts' which shall be non-empty
--               :  result is list of BTree
--               :  result list is empty: acts is not a possible action
--               :  result contains empty list: STOP is possible after state


afterBTree :: (Set.Set (ChanId,[Const])) -> BTree -> IOE [BTree]
afterBTree acts btree  =  do
     newbtrees <- mapM (afterBBranch acts) btree
     return $ concat newbtrees


afterBBranch :: (Set.Set (ChanId,[Const])) -> BBranch -> IOE [BTree]

afterBBranch acts (BTpref btoffs [] preds next)  =  do
     match <- matchAct2CTOffer acts btoffs
     case match of
     { Nothing    -> do return $ []
     ; Just iwals -> let preds' = map (partSubst (Map.map cstrConst iwals)) preds in do   -- should compSubst be used?
                         preds'' <- mapM eval preds'  
                         let preds''' = map cstrConst preds''
                             assertions = foldr add Solve.empty preds''' in do
                                    smtEnv <- getSMT "current"
                                    (sat,smtEnv') <- lift $ runStateT (uniSolve [] assertions) smtEnv
                                    putSMT "current" smtEnv'
                                    case sat of
                                    { Unsolvable -> do return $ []
                                    ; Solved sol -> do cnode <- return $ nextNode (iwals`Map.union`sol) next
                                                       after <- unfold cnode
                                                       return $ [after]
                                    ; UnableToSolve -> error "TXS Primer after: No hidden variables -> unexpected case."
                                    }
     }

afterBBranch acts (BTpref btoffs hidvars preds next)  =  do
     match <- matchAct2CTOffer acts btoffs
     case match of
     { Nothing    -> do return $ []
     ; Just iwals -> do preds' <- return $ map (partSubst (Map.map cstrConst iwals)) preds
                        let assertions = foldr add Solve.empty preds' in do
                            smtEnv <- getSMT "current"
                            (sat,smtEnv') <- lift $ runStateT (uniSolve hidvars assertions) smtEnv
                            putSMT "current" smtEnv'
                            case sat of
                            { Unsolvable -> do return $ []
                            ; Solved sol -> do cnode <- return $ nextNode (iwals`Map.union`sol) next
                                               after <- unfold cnode
                                               return $ [after]
                            ; UnableToSolve -> do lift $ hPutStrLn stderr $ "TXS Primer after: " ++
                                                         "Hidden variables not unique\n"
                                                  return $ []
                            }
     }

afterBBranch acts (BTtau btree)  =  do
     afterBTree acts btree


-- ----------------------------------------------------------------------------------------- --
-- afterQui      :  possible BTree states of bTree after quiescence
--               :  result is list of BTree
--               :  result list is empty: quiescence is not possible
--               :  result contains empty list: STOP is possible after quiescence
-- isQuiBBranch  :  is bbranch directly quiescent


afterQui :: BTree -> IOE [BTree]
afterQui btree  =  do
     topQuis    <- mapM isQuiBBranch btree
     deepAfters <- mapM afterQui [ btree' | BTtau btree' <- btree ]
     if  and topQuis
       then do
         return $ [ btree ]
       else do
         return $ concat deepAfters


isQuiBBranch :: BBranch -> IOE Bool

isQuiBBranch (BTpref btoffs hidvars preds next)  =  do
     isIn  <- isInCTOffers  btoffs
     isOut <- isOutCTOffers btoffs
     if  Set.null btoffs
       then do
         lift $ hPutStrLn stderr $ "TXS Primer isQuiBBranch: empty btoffers\n"
         return $ True
       else
         if  isIn
           then do
             return $ True
           else
             if  isOut
               then do
                 vvars <- return $ concat $ map ctchoffers (Set.toList btoffs)
                 ivars <- return $ vvars ++ hidvars
                 if  null ivars
                   then do
                     predEval <- evalCnrs preds
                     return $ not predEval
                   else
                     -- should preds be simplified using evaluation?
                     let assertions = foldr add Solve.empty preds in do
                     smtEnv <- getSMT "current"
                     (sat,smtEnv') <- lift $ runStateT (satSolve ivars assertions) smtEnv
                     putSMT "current" smtEnv'
                     case sat of
                     { Sat     -> do return $ False
                     ; Unsat   -> do return $ True
                     ; Unknown -> do lift $ hPutStrLn stderr
                                          $ error $ "TXS Primer afterQuiBBranch: Unknown Qui\n"
                                     return $ True
                     }
               else do
                 lift $ hPutStrLn stderr
                      $ "TXS Primer isQuiBBranch: Unknown Qui because offer not I/O\n"
                 return $ True

isQuiBBranch (BTtau btree)  =  do
     return $ False


-- ----------------------------------------------------------------------------------------- --
-- matchAct2CTOffer     :  match a set of CTOffers with values of an action
--   result = Nothing   :  no instantiation possible
--   result = Just map  :  matching map; can be empty: only gates; no values


matchAct2CTOffer :: (Set.Set (ChanId,[Const])) -> (Set.Set CTOffer) -> IOE (Maybe IWals)
matchAct2CTOffer acts ctoffs
  =  if  Set.null ctoffs
       then do
         lift $ hPutStrLn stderr $ "TXS Primer matchAct2CTOffer: empty ctoffs\n"
         return $ Nothing
       else
         if  Set.map (sig . IdChan . fst ) acts == Set.map (sig . IdChan . ctchan) ctoffs
           then do
             return $ Just $ Map.fromList $ concat [ zip ctchoffs wals
                                                   | CToffer chid1 ctchoffs <- Set.toList ctoffs
                                                   , ( chid2, wals )        <- Set.toList acts
                                                   , sig (IdChan chid1) == sig (IdChan chid2)
                                                   ]
           else do
             return $ Nothing


-- ----------------------------------------------------------------------------------------- --
-- helper functions
--
-- ----------------------------------------------------------------------------------------- --
-- isInCTOffers  :  is input offer?

isInCTOffers :: (Set.Set CTOffer) -> IOE Bool
isInCTOffers ctoffers  =  do
     DefModel (ModelDef chinsets choutsets _ bexp) <- getSpec
     chinset  <- return $ Set.unions chinsets
     choutset <- return $ Set.unions choutsets
     chanids  <- return $ Set.map ctchan ctoffers
     return $    not (Set.null (chanids `Set.intersection` chinset))
              &&     (Set.null (chanids `Set.intersection` choutset))


-- ----------------------------------------------------------------------------------------- --
-- isOutCTOffers :  is output offer?

isOutCTOffers :: (Set.Set CTOffer) -> IOE Bool
isOutCTOffers ctoffers  =  do
     DefModel (ModelDef chinsets choutsets _ bexp) <- getSpec
     chinset  <- return $ Set.unions chinsets
     choutset <- return $ Set.unions choutsets
     chanids  <- return $ Set.map ctchan ctoffers
     return $        (Set.null (chanids `Set.intersection` chinset))
              && not (Set.null (chanids `Set.intersection` choutset))


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

