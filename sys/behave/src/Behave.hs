{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set

-- import from local
import           Next
import           Reduce
import           Unfold

-- import from behavedef
import           BTree

-- import from behaveenv
import qualified EnvBTree            as IOB

-- import from coreenv
import qualified EnvData

-- import from defs
import qualified TxsDefs

-- import from solve
import           Solve
import           SolveDefs

-- import from value
import qualified Eval


-- ----------------------------------------------------------------------------------------- --
-- behInit :  initialize BTree


behInit :: [ Set.Set TxsDefs.ChanId ] -> TxsDefs.BExpr -> IOB.IOB (Maybe BTree)
behInit chsets bexp  =  do
     btree' <- unfold chsets (BNbexpr Map.empty bexp)
     return $ Just btree'



-- | behMayMenu :  may menu of BTree without quiescence
--
-- Returns the list of all possible *visible* symbolic-actions.
behMayMenu :: [ Set.Set TxsDefs.ChanId ] -- ^
           -> BTree
           -> Menu
behMayMenu chsets btree'
  =  [ ( btoffs, hidvars, pred' ) | BTpref btoffs hidvars pred' _ <- btree' ]
     ++ concat [ behMayMenu chsets btree'' | BTtau btree'' <- btree' ]


-- ----------------------------------------------------------------------------------------- --
-- behMustMenu :  must menu of BTree without quiescence


behMustMenu :: [ Set.Set TxsDefs.ChanId ] -> BTree -> Menu
behMustMenu _ _
  =  []

--   TODO

{-   case [ btree' | BTtau btree' <- btree ] of
     { []      -> [ ( btoffs, hidvars, preds ) | BTpref btoffs hidvars preds next <- btree ]
     ; btrees' -> concat $ map behMustMenu btrees'
-}


-- ----------------------------------------------------------------------------------------- --
-- behRefusal :  check refusal set on BTree
--
-- TODO: Put an informal/description of refusal set.
--
behRefusal :: BTree -> Set.Set TxsDefs.ChanId -> Bool
behRefusal bt refset
  =  case [ bt' | BTtau bt' <- bt ] of
     { []      -> and [ refBBranch bbranch refset | bbranch <- bt ]
     ; btrees' -> any (`behRefusal` refset) btrees'
     }


refBBranch :: BBranch -> Set.Set TxsDefs.ChanId -> Bool

refBBranch (BTpref btoffs _ _ _) refset
  =  not $ Set.map ctchan btoffs `Set.isSubsetOf` refset

refBBranch (BTtau _) _
  =  False


-- ----------------------------------------------------------------------------------------- --
-- behAfterAct :  perform after action on BTree
--

behAfterAct :: [ Set.Set TxsDefs.ChanId ] -> BTree -> BehAction -> IOB.IOB (Maybe BTree)
behAfterAct chsets bt behact
 | Set.null behact  =  do
     IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "behAfterAct: after empty set/tau action" ]
     return Nothing
 | otherwise = do
     afters <- afterActBTree chsets behact bt
     if  null afters
       then return Nothing
       else do let newbtree  = map BTtau afters
               newbtree' <- reduce newbtree
               return $ Just newbtree'


-- ----------------------------------------------------------------------------------------- --
-- afterActBTree :  list of possible BTree states of btree after non-empty behact
--               :  result list is empty: behact is not a possible action
--               :  result contains empty list: STOP is possible after state


afterActBTree :: [ Set.Set TxsDefs.ChanId ] -> BehAction -> BTree -> IOB.IOB [BTree]
afterActBTree chsets behact bt  =  do
     newbtrees <- mapM (afterActBBranch chsets behact) bt
     return $ concat newbtrees


-- ----------------------------------------------------------------------------------------- --
-- afterBBranch  :  list of possible BTree states of BBranch after non-empty behact
--               :  result list is empty: behact is not a possible action
--               :  result contains empty list: STOP is possible after state


afterActBBranch :: [ Set.Set TxsDefs.ChanId ] -> BehAction -> BBranch -> IOB.IOB [BTree]

afterActBBranch chsets behact (BTpref btoffs [] pred' next)  =  do
     match <- matchAct2CTOffer behact btoffs
     case match of
       Nothing    -> return []
       Just iwals -> do
                      tds <- gets IOB.tdefs
                      let pred'' = TxsDefs.subst (Map.map TxsDefs.cstrConst iwals) (TxsDefs.funcDefs tds) pred'
                      condition <- Eval.eval pred''
                      case condition of
                          TxsDefs.Cbool True  -> do let cnode = nextNode iwals next
                                                    after <- unfold chsets cnode
                                                    return [after]
                          TxsDefs.Cbool False -> return []
                          _                   -> error "afterActBBranch - condition is not a Boolean"

afterActBBranch chsets behact (BTpref btoffs hidvars pred' next)  =  do
     match <- matchAct2CTOffer behact btoffs
     case match of
       Nothing    -> return []
       Just iwals -> do
                      tds <- gets IOB.tdefs
                      let pred'' = TxsDefs.subst (Map.map TxsDefs.cstrConst iwals) (TxsDefs.funcDefs tds) pred'
                          assertion = add pred'' empty
                      smtEnv <- IOB.getSMT "current"
                      (sat,smtEnv') <- lift $ runStateT (uniSolve hidvars assertion) smtEnv
                      IOB.putSMT "current" smtEnv'
                      case sat of
                        Unsolvable -> return []
                        Solved sol -> do let cnode = nextNode (iwals `Map.union` sol) next
                                         after <- unfold chsets cnode
                                         return [after]
                        UnableToSolve -> do IOB.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                                          "after: cannot find unique value for hidden variables" ]
                                            return []

afterActBBranch chsets behact (BTtau bt)  =  afterActBTree chsets behact bt


-- ----------------------------------------------------------------------------------------- --
-- matchAct2CTOffer     :  match a set of CTOffers with values of an action
--   result = Nothing   :  no instantiation possible
--   result = Just map  :  matching map; can be empty: only gates, no values


matchAct2CTOffer :: BehAction -> Set.Set CTOffer -> IOB.IOB (Maybe IWals)
matchAct2CTOffer behact ctoffs
    | Set.null ctoffs =
        do
          IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "matchAct2CTOffer: empty ctoffs" ]
          return Nothing
    | Set.map fst behact == Set.map ctchan ctoffs =
        return $ Just $ Map.fromList
                              $ concat [ zip ctchoffs wals
                                       | CToffer chid1 ctchoffs <- Set.toList ctoffs
                                       , ( chid2, wals )        <- Set.toList behact
                                       , chid1 == chid2
                                       ]
    | otherwise =
        return Nothing

-- ----------------------------------------------------------------------------------------- --
-- behAfterRef :  perform after refusal on BTree


behAfterRef :: BTree -> Set.Set TxsDefs.ChanId -> IOB.IOB (Maybe BTree)
behAfterRef bt refset  =  do
     afters <- afterRefBTree refset bt
     if  null afters
       then return Nothing
       else do let newbtree = map BTtau afters
               newbtree' <- reduce newbtree
               return $ Just newbtree'

-- ----------------------------------------------------------------------------------------- --
-- afterRefBTree :  list of possible BTree states after refusal
--               :  result list is empty: refusal is not possible
--               :  result contains empty list: STOP is possible after refusal


afterRefBTree :: Set.Set TxsDefs.ChanId -> BTree -> IOB.IOB [BTree]
afterRefBTree refset bt =
    case [ bt' | BTtau bt' <- bt ] of
      []      -> if and [ refBBranch bbranch refset | bbranch <- bt ]
                   then return [bt]
                   else return []
      btrees' -> do btrees'' <- mapM (afterRefBTree refset) btrees'
                    return $ concat btrees''

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
