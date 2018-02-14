{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ----------------------------------------------------------------------------------------- --

module SBehave

-- ----------------------------------------------------------------------------------------- --
--
-- Test Primitives over LTS to BTree -- no IO
--
-- ----------------------------------------------------------------------------------------- --
-- export

( behInit      -- :: [ Set.Set TxsDefs.ChanId ] -> TxsDefs.BExpr -> IOC.IOC (Maybe BTree)
               -- initialize BTree
, behMayMenu   -- :: [ Set.Set TxsDefs.ChanId ] -> BTree -> Menu
               -- may menu of BTree without quiescence
, behMustMenu  -- :: [ Set.Set TxsDefs.ChanId ] -> BTree -> Menu
               -- must menu of BTree without quiescence
, behRefusal   -- :: BTree -> Set.Set TxsDefs.ChanId -> Bool
               -- check refusal set on BTree
, behAfterAct  -- :: [ Set.Set TxsDefs.ChanId ] -> BTree -> BehAction -> IOC.IOC (Maybe BTree)
               -- perform after action on BTree
, behAfterRef  -- :: BTree -> Set.Set TxsDefs.ChanId -> IOC.IOC (Maybe BTree)
               -- perform after refusal on BTree
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State

import qualified Data.Map            as Map
import qualified Data.Set            as Set

-- import from local
import           SNext
--import           Reduce
--import           Unfold
import           SExpand

-- import from behavedef
import           STree

-- import from behaveenv
import TreeVars
import qualified EnvSTree            as SEE

-- import from core
-- import qualified CoreUtils

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData

-- import from defs
import qualified TxsDefs
import qualified Sigs

-- import from solve
import           Solve
import           SolveDefs

-- import from valexpr
import           ConstDefs
import           ValExpr

-- ----------------------------------------------------------------------------------------- --
-- behInit :  initialize BTree


behInit :: [ Set.Set TxsDefs.ChanId ] -> TxsDefs.BExpr -> IOC.IOC (Maybe Det)
behInit chsets bexp  =  do
    envb <- filterEnvCtoEnvE
    let stree = evalState (expand chsets (SNbexpr Map.empty bexp)) envb
    --writeEnvEtoEnvC envb'
    return $ Just (determinize stree)



-- | behMayMenu :  may menu of BTree without quiescence
--
-- Returns the list of all possible *visible* symbolic-actions.
behMayMenu :: [ Set.Set TxsDefs.ChanId ] -- ^
           -> [STree]
           -> Menu
behMayMenu _ trees
    = [ (offs, hids, cnd) | tree <- trees, (STtrans offs hids cnd _) <- sttrans tree ]
{-behMayMenu chsets btree'
  =  [ ( btoffs, hidvars, pred' ) | BTpref btoffs hidvars pred' _ <- btree' ]
     ++ concat [ behMayMenu chsets btree'' | BTtau btree'' <- btree' ]
-}

-- ----------------------------------------------------------------------------------------- --
-- behMustMenu :  must menu of BTree without quiescence

behMustMenu :: [ Set.Set TxsDefs.ChanId ] -> STree -> Menu
behMustMenu _ _
  =  []
{-   case [ btree' | BTtau btree' <- btree ] of
     { []      -> [ ( btoffs, hidvars, preds ) | BTpref btoffs hidvars preds next <- btree ]
     ; btrees' -> concat $ map behMustMenu btrees'
-}


-- ----------------------------------------------------------------------------------------- --
-- behRefusal :  check refusal set on BTree
--
behRefusal :: STree -> Set.Set TxsDefs.ChanId -> Bool
behRefusal = error "behRefusal: not implemented yet"
{-behRefusal :: BTree -> Set.Set TxsDefs.ChanId -> Bool
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
-}

-- ----------------------------------------------------------------------------------------- --
-- behAfterAct :  perform after action on BTree
--

behAfterAct :: [ Set.Set TxsDefs.ChanId ] -> [STree] -> BehAction -> IOC.IOC (Maybe [STree])
behAfterAct chsets strees behact
--behAfterAct _ _ behact
 | Set.null behact  =  do
     IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "behAfterAct: after empty set/tau action" ]
     return Nothing
-- | otherwise = error "not implemented yet"
 | otherwise = do
     afters <- mapM (afterSBranch chsets behact) $ concat (sttrans <$> strees)
     return $ if null afters
       then Nothing
       else Just $ concat afters
--       else do let newbtree  = map BTtau afters
--               newbtree' <- reduce newbtree
--               newbtree' <- return newbtree
--               return $ Just newbtree'

-- ----------------------------------------------------------------------------------------- --
-- afterActBTree :  list of possible BTree states of btree after non-empty behact
--               :  result list is empty: behact is not a possible action
--               :  result contains empty list: STOP is possible after state

{-
afterActBTree :: [ Set.Set TxsDefs.ChanId ] -> BehAction -> BTree -> IOC.IOC [BTree]
afterActBTree chsets behact bt  =  do
     newbtrees <- mapM (afterActBBranch chsets behact) bt
     return $ concat newbtrees


-- ----------------------------------------------------------------------------------------- --
-- afterBBranch  :  list of possible BTree states of BBranch after non-empty behact
--               :  result list is empty: behact is not a possible action
--               :  result contains empty list: STOP is possible after state
-}

afterSBranch :: [ Set.Set TxsDefs.ChanId ] -> BehAction -> STtrans -> IOC.IOC [STree]
afterSBranch chsets behact (STtrans offs [] cnd next) = do
    match <- matchAct2CTOffer behact offs
    case match of
        Nothing -> return []
        Just iwals -> do
            --tdefs <- gets IOC.tdefs
            --let tstate = IOC.state 
            --let fdefs = TxsDefs.funcDefs tdefs
            fdefs <- (TxsDefs.funcDefs . IOC.tdefs) <$> gets IOC.state
            let cndVal = subst (Map.map cstrConst iwals) fdefs cnd
            case ValExpr.eval cndVal of
                Right (Cbool True)  -> do
                    let next' = updateNode fdefs iwals (stnode next)
                    envb <- filterEnvCtoEnvE
                    let after = evalState (expand chsets next') envb
                    return [after]
                Right (Cbool False) -> return []
                Right _             -> do
                    IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                      "afterActBBranch - condition is not a Boolean value"]
                    return []
                Left s              -> do
                    IOC.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                     ("afterActBBranch - condition is not a value - " ++ show s)]
                    return []

afterSBranch chsets behact (STtrans offs hids cnd next)  =  do
    match <- matchAct2CTOffer behact offs
    case match of
        Nothing    -> return []
        Just iwals -> do
            fdefs <- (TxsDefs.funcDefs . IOC.tdefs) <$> gets IOC.state
            let cndVal = subst (Map.map cstrConst iwals) fdefs cnd
                assertion = add cndVal empty
            smtEnv <- IOC.getSMT "current"
            (sat,smtEnv') <- lift $ runStateT (uniSolve hids assertion) smtEnv
            IOC.putSMT "current" smtEnv'
            case sat of
                Unsolvable -> return []
                Solved sol -> do
                    envb <- filterEnvCtoEnvE
                    let iwals' = iwals `Map.union` sol
                    let update = updateNode fdefs iwals'
                        next' = update $ stnode next
                        after = evalState (expand chsets next') envb
                    return [after]
                UnableToSolve -> do
                    IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                    "after: cannot find unique value for hidden variables" ]
                    return []

{-

afterActBBranch chsets behact (BTtau bt)  =  afterActBTree chsets behact bt

-}

-- ----------------------------------------------------------------------------------------- --
-- matchAct2CTOffer     :  match a set of CTOffers with values of an action
--   result = Nothing   :  no instantiation possible
--   result = Just map  :  matching map; can be empty: only gates, no values


matchAct2CTOffer :: BehAction -> Set.Set CTOffer -> IOC.IOC (Maybe IWals)
matchAct2CTOffer behact ctoffs
    | Set.null ctoffs =
        do
          IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "matchAct2CTOffer: empty ctoffs" ]
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

behAfterRef :: STree -> Set.Set TxsDefs.ChanId -> SEE.SEE (Maybe STree)
behAfterRef = error "behAfterRef: not implemented yet"

{-
behAfterRef :: BTree -> Set.Set TxsDefs.ChanId -> IOC.IOC (Maybe BTree)
behAfterRef bt refset  =  do
     afters <- afterRefBTree refset bt
     if  null afters
       then return Nothing
       else do let newbtree = map BTtau afters
               --newbtree' <- reduce newbtree
               newbtree' <- return newbtree
               return $ Just newbtree'
-}

-- ----------------------------------------------------------------------------------------- --
-- afterRefBTree :  list of possible BTree states after refusal
--               :  result list is empty: refusal is not possible
--               :  result contains empty list: STOP is possible after refusal

{-
afterRefBTree :: Set.Set TxsDefs.ChanId -> BTree -> IOC.IOC [BTree]
afterRefBTree refset bt =
    case [ bt' | BTtau bt' <- bt ] of
      []      -> if and [ refBBranch bbranch refset | bbranch <- bt ]
                   then return [bt]
                   else return []
      btrees' -> do btrees'' <- mapM (afterRefBTree refset) btrees'
                    return $ concat btrees''
-}
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

-- ----------------------------------------------------------------------------------------- --
-- filterEnvCtoEnvE

filterEnvCtoEnvE :: IOC.IOC SEE.EnvE
filterEnvCtoEnvE = do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> return SEE.EnvE { SEE.tdefs    = TxsDefs.empty
                            , SEE.sigs     = Sigs.empty
                            , SEE.stateid  = 0
                            , SEE.params   = IOC.params envc
                            , SEE.msgs     = []
                            }
       IOC.Initing{..}
         -> return SEE.EnvE { SEE.tdefs    = tdefs
                            , SEE.sigs     = sigs
                            , SEE.stateid  = 0
                            , SEE.params   = IOC.params envc
                            , SEE.msgs     = []
                            }
       IOC.Testing{..}
         -> return SEE.EnvE { SEE.tdefs    = tdefs
                            , SEE.sigs     = sigs
                            , SEE.stateid  = curstate
                            , SEE.params   = IOC.params envc
                            , SEE.msgs     = []
                            }
       IOC.Simuling{..}
         -> return SEE.EnvE { SEE.tdefs    = tdefs
                            , SEE.sigs     = sigs
                            , SEE.stateid  = curstate
                            , SEE.params   = IOC.params envc
                            , SEE.msgs     = []
                            }
       IOC.Stepping{..}
         -> return SEE.EnvE { SEE.tdefs    = tdefs
                            , SEE.sigs     = sigs
                            , SEE.stateid  = curstate
                            , SEE.params   = IOC.params envc
                            , SEE.msgs     = []
                            }

