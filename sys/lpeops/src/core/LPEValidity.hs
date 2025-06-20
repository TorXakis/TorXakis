{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEValidity
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns        #-}
module LPEValidity (
validateLPE,
validateLPEModel,
validateLPESummand,
validateValExpr,
validateSortList,
validateSortPair
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified EnvCore as IOC
import qualified EnvData
import qualified SortOf
import qualified TxsDefs
import qualified CstrId
import qualified FuncId
import qualified SortId
--import qualified ChanId
import qualified VarId
import           Constant hiding (args, sort)
import           ValExpr
import           LPETypes
import           ValExprVisitor

validateLPE :: LPEOperation
validateLPE lpe _out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<valid>>" ]
    let problems = validateLPEModel lpe
    if problems /= []
    then return (Left problems)
    else do IOC.putMsgs [ EnvData.TXS_CORE_ANY "No problems detected in LPE!" ]
            return (Right lpe)
-- validateLPE

-- This method can detect certain problems with an LPE, making finding bugs in LPE operations easier:
validateLPEModel :: LPE -> [String]
validateLPEModel lpe =
    let summands = Set.toList (lpeSummands lpe) in
      concatMap getSmdProblems (zip [1..] summands) ++ getChanProblems
  where
    getSmdProblems :: (Int, LPESummand) -> [String]
    getSmdProblems (_i, smd) = validateLPESummand ("summand " ++ show smd) (lpeParams lpe) smd
    
    getChanProblems :: [String]
    getChanProblems = []
        --let overlap = Set.intersection (lpeInChans lpe) (lpeOutChans lpe) in
        --  [ "Channel " ++ Text.unpack (ChanId.name c) ++ " is both an input channel and an output channel!" | c <- Set.toList overlap ]
-- validateLPEModel

validateLPESummand :: String -> Set.Set VarId.VarId -> LPESummand -> [String]
validateLPESummand location scope summand =
    let newScope = Set.union scope (lpeSmdVarSet summand) in
      getDisjointScopeProblems ++ validateValExpr ("guard of " ++ location) newScope (lpeSmdGuard summand) ++ getProcInstProblems newScope
  where
    getDisjointScopeProblems :: [String]
    getDisjointScopeProblems =
        let sharedVars = Set.toList (Set.intersection scope (lpeSmdVarSet summand)) in
          map (\v -> "Variable " ++ Text.unpack (VarId.name v) ++ " is redeclared in " ++ location ++ "!") sharedVars
    -- getDisjointScopeProblems
    
    getProcInstProblems :: Set.Set VarId.VarId -> [String]
    getProcInstProblems s =
        let nonExistentParameters = Map.keysSet (lpeSmdEqs summand) Set.\\ scope in
          map (\p -> "Parameter " ++ Text.unpack (VarId.name p) ++ " is not assigned in process instantiation of " ++ location ++ "!") (Set.toList nonExistentParameters)
          ++
          concatMap (validateValExpr ("process instantiation of " ++ location) s . snd) (Map.toList (lpeSmdEqs summand))
-- validateLPESummand

-- Given a data expression, this method lists any problems that may exist that would indicate that the input is invalid.
validateValExpr :: String -> Set.Set VarId.VarId -> TxsDefs.VExpr -> [String]
validateValExpr location scope xpr = customData (visitValExpr getProblemsVisitor xpr)
  where
    getProblemsVisitor :: [ValExprVisitorOutput [String]] -> TxsDefs.VExpr -> ValExprVisitorOutput [String]
    getProblemsVisitor subExps expr =
        let problems = case expr of
                        (view -> Vconst (Cbool _))        -> []
                        (view -> Vconst (Cint _))         -> []
                        (view -> Vconst (Cstring _))      -> []
                        (view -> Vconst (Cregex _))       -> []
                        -- Constructor signatures must be used properly in constant expressions:
                        (view -> Vconst (Ccstr cid args)) ->
                            validateSortList ("constant \"" ++ Text.unpack (CstrId.name cid) ++ "\" constructor in " ++ location) (CstrId.cstrargs cid) (map SortOf.sortOf args)
                        (view -> Vconst (Cany _))         -> []
                        -- Variables that are referenced must exist in the current scope:
                        (view -> Vvar vid)                ->
                            let alternatives = "{" ++ List.intercalate ", " (map show (Set.toList scope)) ++ "}" in
                              --["Variable \"" ++ Text.unpack (VarId.name vid) ++ "\" is not in scope for " ++ location ++ "; did you mean one of " ++ alternatives ++ "?" | Set.notMember vid scope]
                              ["Variable \"" ++ show vid ++ "\" is not in scope for " ++ location ++ "; did you mean one of " ++ alternatives ++ "?" | Set.notMember vid scope]
                        -- Function signatures must be used properly:
                        (view -> Vfunc fid args)          ->
                            validateSortList ("function \"" ++ Text.unpack (FuncId.name fid) ++ "\" call in " ++ location) (FuncId.funcargs fid) (map SortOf.sortOf args)
                        -- Constructor signatures must be used properly:
                        (view -> Vcstr cid args)          ->
                            validateSortList ("\"" ++ Text.unpack (CstrId.name cid) ++ "\" constructor in " ++ location) (CstrId.cstrargs cid) (map SortOf.sortOf args)
                        -- Recognizer signatures must be used properly:
                        (view -> Viscstr cid arg)         ->
                            validateSortPair ("recognizer \"is" ++ Text.unpack (CstrId.name cid) ++ "\" call in " ++ location) (CstrId.cstrsort cid) (SortOf.sortOf arg)
                        -- Accessor signatures must be used properly:
                        (view -> Vaccess cid n _ arg)     ->
                            validateSortPair ("accessor \"" ++ Text.unpack n ++ "\" call in " ++ location) (CstrId.cstrsort cid) (SortOf.sortOf arg)
                        (view -> Vite{})                  -> [] -- TODO
                        (view -> Vdivide _ _)             -> [] -- TODO
                        (view -> Vmodulo _ _)             -> [] -- TODO
                        (view -> Vgez _)                  -> [] -- TODO
                        (view -> Vsum _)                  -> [] -- TODO
                        (view -> Vproduct _)              -> [] -- TODO
                        (view -> Vequal _ _)              -> [] -- TODO
                        (view -> Vand _)                  -> [] -- TODO
                        (view -> Vnot _)                  -> [] -- TODO
                        (view -> Vlength _)               -> [] -- TODO
                        (view -> Vat _ _)                 -> [] -- TODO
                        (view -> Vconcat _)               -> [] -- TODO
                        (view -> Vstrinre _ _)            -> [] -- TODO
                        -- Signatures of predefined functions must be used properly:
                        (view -> Vpredef _ fid args)      ->
                            validateSortList ("predefined function \"" ++ Text.unpack (FuncId.name fid) ++ "\" call in " ++ location) (FuncId.funcargs fid) (map SortOf.sortOf args)
        in ValExprVisitorOutput expr 1 (concatMap customData subExps ++ problems)
-- getValExprProblems

-- Given two lists of sorts (where the one usually comes from a list of parameters and the other from a list of parameter *values*),
-- this method lists any incompatibility problems, such as:
--  - The sort at position i in the first list does not match the sort at position i in the second list; or
--  - The size of the first list is not equal to the size of the second list.
validateSortList :: String -> [SortId.SortId] -> [SortId.SortId] -> [String]
validateSortList location = getProblems 1
  where
    getProblems :: Int -> [SortId.SortId] -> [SortId.SortId] -> [String]
    getProblems _ [] [] = []
    getProblems n (_:remainingExpected) [] = ["Expected " ++ show (n + length remainingExpected) ++ " arguments in " ++ location ++ ", found " ++ show (n - 1) ++ "!"]
    getProblems n [] (_:remainingFound) = ["Expected " ++ show (n - 1) ++ " arguments in " ++ location ++ ", found " ++ show (n + length remainingFound) ++ "!"]
    getProblems n (e:remainingExpected) (f:remainingFound) = validateSortPair ("argument " ++ show n ++ " in " ++ location) e f ++ getProblems (n + 1) remainingExpected remainingFound
-- validateSortList

-- Given two sorts (where the one usually is the sort of a parameter and the other the sort of a parameter *value*),
-- this method lists any incompatibility problems, in particular whether the two sorts are not equal.
validateSortPair :: String -> SortId.SortId -> SortId.SortId -> [String]
validateSortPair location expected found =
    ["Sort mismatch in " ++ location ++ "; found " ++ Text.unpack (SortId.name found) ++ " but expected " ++ Text.unpack (SortId.name expected) ++ "!" | found /= expected]
-- validateSortPair


