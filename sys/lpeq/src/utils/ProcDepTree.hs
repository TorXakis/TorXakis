{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ProcDepTree
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}

module ProcDepTree (
ProcDepTree(..),
getProcDepTreeProblems,
getProcDepTree,
getMaxDepthPerProc,
getProcsOrderedByMaxDepth
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified EnvCore as IOC
import qualified TxsDefs
import qualified TxsShow
import qualified ProcId
import qualified ProcDef
import qualified ChanId
import BehExprDefs
import ProcIdFactory
import ActOfferFactory

import ProcSearch
import ThreadUtils

data ProcDepTree = Uninitialized
                 | Branch ProcId.ProcId String [ProcDepTree]
                 | Circular ProcId.ProcId String
                 | InfiniteLoop ProcId.ProcId
                   deriving (Eq, Ord)
-- ProcDepTree

showProcDepTree :: String -> String -> ProcDepTree -> [String]
showProcDepTree pidPrefix _depsPrefix Uninitialized = [pidPrefix ++ "UNINIT"]
showProcDepTree pidPrefix depsPrefix (Branch ownerPid location dependencies) = (pidPrefix ++ TxsShow.fshow ownerPid ++ " (" ++ location ++ ")") : showProcDepTreeDeps depsPrefix dependencies
showProcDepTree pidPrefix _depsPrefix (Circular ownerPid location) = [pidPrefix ++ "CIRCULAR " ++ TxsShow.fshow ownerPid ++ " (" ++ location ++ ")"]
showProcDepTree pidPrefix _depsPrefix (InfiniteLoop ownerPid) = [pidPrefix ++ "INFLOOP " ++ TxsShow.fshow ownerPid]

showProcDepTreeDeps :: String -> [ProcDepTree] -> [String]
showProcDepTreeDeps _depsPrefix [] = []
showProcDepTreeDeps depsPrefix deps =
    let depsStrs = concatMap (showProcDepTree (depsPrefix ++ "|-") (depsPrefix ++ "| ")) (List.init deps) in
    let lastStrs = showProcDepTree (depsPrefix ++ "\\-") (depsPrefix ++ ". ") (List.last deps) in
      depsStrs ++ lastStrs
-- showProcDepTreeDeps

instance Show ProcDepTree where
    show = List.intercalate "\n" . showProcDepTree "" ""
-- Show ProcDepTree

getProcDepTreeProblems :: TxsDefs.BExpr -> IOC.IOC [String]
getProcDepTreeProblems startBExpr = do
    tree <- getProcDepTree startBExpr
    let problems = getProblems tree
    if null problems
    then do procStrs <- showProcsInBExpr startBExpr
            IOC.putInfo (showProcDepTree "" "" tree ++ procStrs)
            return []
    else do let problemsStrs = ["Encountered problems while constructing process dependency tree:"] ++ map ("|-" ++) (List.init problems) ++ ["\\-" ++ List.last problems]
            let treeStrs = "Process dependency tree:" : showProcDepTree "" "" tree
            procStrs <- showProcsInBExpr startBExpr
            return (problemsStrs ++ treeStrs ++ procStrs)
  where
    getProblems :: ProcDepTree -> [String]
    getProblems Uninitialized = ["Tree contains uninitialized branches!"]
    getProblems (Branch _ _ dependencies) = concatMap getProblems dependencies
    getProblems (Circular pid _) = ["Circular dependency related to process " ++ TxsShow.fshow pid ++ "!"]
    getProblems (InfiniteLoop pid) = ["Process calls to process " ++ TxsShow.fshow pid ++ " that are not separated by any action!"]
-- getProcDepTree

-- Builds the process dependency tree for a given behavioral expression.
-- Depends on PBranchInst having been applied first.
getProcDepTree :: TxsDefs.BExpr -> IOC.IOC ProcDepTree
getProcDepTree currentBExpr = head <$> getParTrees (Nothing, Set.empty, Set.empty, Set.empty) "Root" currentBExpr

type Visited = ( Maybe ProcId.ProcId          -- Parent process.
               , Set.Set ProcId.ProcId        -- Thread processes that were visited.
               , Set.Set ProcId.ProcId        -- Sequential processes that were visited (in the current thread process) WITHOUT having seen an action afterwards.
               , Set.Set ProcId.ProcId        -- Sequential processes that were visited (in the current thread process) with a subsequent action.
               )
-- Visited

getSeqTrees :: Visited -> String -> TxsDefs.BExpr -> IOC.IOC [ProcDepTree]
getSeqTrees visited@(parentParProc, parProcs, seqProcs, seqProcsAfterAct) location currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid _cids _vexprs)
            -- Detect (and avoid) infinite recursion:
          | Set.member pid parProcs -> return [Circular pid location]
          | Set.member pid seqProcs -> return [InfiniteLoop pid]
          | Set.member pid seqProcsAfterAct -> return []
          | otherwise -> do r <- getProcById pid
                            case r of
                              Just (ProcDef.ProcDef _cids _vids body) ->
                                  getSeqTrees (parentParProc, parProcs, Set.insert pid seqProcs, seqProcsAfterAct) ("Body of " ++ TxsShow.fshow pid) body
                              Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
      (TxsDefs.view -> Choice bexprs) ->
          concat <$> Monad.mapM (getSeqTrees visited "Sub-expression of Choice") (Set.toList bexprs)
      (TxsDefs.view -> Parallel cidSet bexprs) ->
          concat <$> Monad.mapM (getParTrees visited ("Sub-expression of Parallel " ++ showCidSet cidSet)) bexprs
      (TxsDefs.view -> Guard _guard bexpr) ->
          getTrees currentBExpr 0 visited "Successor of Guard" bexpr
      (TxsDefs.view -> ActionPref actOffer bexpr) ->
          getTrees currentBExpr 0 visited ("Successor of ActionPref " ++ showCidSet (getActOfferChans actOffer)) bexpr
      (TxsDefs.view -> Hide cidSet bexpr) ->
          getTrees currentBExpr 0 visited ("Successor of Hide " ++ showCidSet cidSet) bexpr
      (TxsDefs.view -> Enable bexpr1 _acceptOffers bexpr2) ->
          getBinTrees "LHS of Enable" bexpr1 "RHS of Enable" bexpr2
      (TxsDefs.view -> Disable bexpr1 bexpr2) ->
          getBinTrees "LHS of Disable" bexpr1 "RHS of Disable" bexpr2
      (TxsDefs.view -> Interrupt bexpr1 bexpr2) ->
          getBinTrees "LHS of Interrupt" bexpr1 "RHS of Interrupt" bexpr2
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
  where
    getBinTrees :: String -> TxsDefs.BExpr -> String -> TxsDefs.BExpr -> IOC.IOC [ProcDepTree]
    getBinTrees location1 bexpr1 location2 bexpr2 = do
        trees1 <- getTrees currentBExpr 0 visited location1 bexpr1
        trees2 <- getTrees currentBExpr 1 visited location2 bexpr2
        return (trees1 ++ trees2)
    -- getBinTrees
    
    showCidSet :: Set.Set ChanId.ChanId -> String
    showCidSet cidSet = "{" ++ List.intercalate ", " (map TxsShow.fshow (Set.toList cidSet)) ++ "}"
-- getSeqTrees

getParTrees :: Visited -> String -> TxsDefs.BExpr -> IOC.IOC [ProcDepTree]
getParTrees (parentParProc, parProcs, seqProcs, seqProcsAfterAct) location currentBExpr =
    case currentBExpr of
      (TxsDefs.view -> ProcInst pid _cids _vexprs)
            -- Detect (and avoid) infinite recursion:
          | Set.member pid parProcs -> return [Circular pid location]
          | Set.member pid seqProcs -> return [InfiniteLoop pid]
          | Set.member pid seqProcsAfterAct -> return [Uninitialized]
          | otherwise -> do r <- getProcById pid
                            case r of
                              Just (ProcDef.ProcDef _cids _vids body) -> do
                                  let parProcs' = case parentParProc of
                                                    Just ppp -> Set.insert ppp parProcs
                                                    Nothing -> Set.empty
                                  seqTrees <- getSeqTrees (Just pid, parProcs', Set.singleton pid, Set.empty) ("Body of " ++ TxsShow.fshow pid) body
                                  return [Branch pid location seqTrees]
                              Nothing -> error ("Unknown process (\"" ++ TxsShow.fshow pid ++ "\")!")
      _ -> error ("Behavioral expression not anticipated (\"" ++ TxsShow.fshow currentBExpr ++ "\")!")
-- getParTrees

getTrees :: TxsDefs.BExpr -> Int -> Visited -> String -> TxsDefs.BExpr -> IOC.IOC [ProcDepTree]
getTrees currentBExpr subExprIndex visited@(parentParProc, parProcs, seqProcs, seqProcsAfterAct) =
    case getSubExprType currentBExpr subExprIndex of
      StpSequential -> getSeqTrees visited
      StpPrefixed -> getSeqTrees (parentParProc, parProcs, Set.empty, Set.union seqProcsAfterAct seqProcs)
      StpStackable -> getParTrees visited
      StpUnsafe -> getParTrees visited
-- getTrees

getMaxDepthPerProc :: ProcDepTree -> Map.Map ProcId.ProcId Int
getMaxDepthPerProc tree = snd (dfs tree)
  where
    dfs :: ProcDepTree -> (Int, Map.Map ProcId.ProcId Int)
    dfs (Branch ownerPid _ []) = (0, Map.singleton ownerPid 0)
    dfs (Branch ownerPid _ dependencies) = let (a, b) = foldl f (0, Map.empty) dependencies in (a + 1, Map.insert ownerPid (a + 1) b)
    dfs _ = (0, Map.empty)
    
    f :: (Int, Map.Map ProcId.ProcId Int) -> ProcDepTree -> (Int, Map.Map ProcId.ProcId Int)
    f soFar t = let (tdepth, tmap) = dfs t in (max (fst soFar) (tdepth + 1), Map.unionWith max (snd soFar) tmap)
-- getMaxDepthPerProc

getProcsOrderedByMaxDepth :: ProcDepTree -> [ProcId.ProcId]
getProcsOrderedByMaxDepth tree = map fst (List.sortOn snd (Map.toList (getMaxDepthPerProc tree)))



