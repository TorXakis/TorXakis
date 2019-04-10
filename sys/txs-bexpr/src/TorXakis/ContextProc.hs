{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for ValExpr: all defined sorts, variables, and functions
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ExistentialQuantification #-}
module TorXakis.ContextProc
( -- * Context for Processes
  ContextProc
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap           as HashMap
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.ContextFunc
import           TorXakis.ProcContext
import           TorXakis.ProcDef
import           TorXakis.ProcSignature

-- | An instance of 'TorXakis.ProcContext'.
data ContextProc = forall c . FuncContext c =>
                            ContextProc { _funcContext :: c
                                          -- procSignatures
                                        , procDefs :: HashMap.Map ProcSignature ProcDef
                                        }

-- | Create ContextProc from SortContext
fromFuncContext :: FuncContext c => c -> ContextProc
fromFuncContext fc = ContextProc fc HashMap.empty

-- | empty
empty :: ContextProc
empty = TorXakis.ContextProc.fromFuncContext TorXakis.ContextFunc.empty

instance SortContext ContextProc where
    -- Can't use
    -- memberSort   = memberSort . sortContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort r (ContextProc ctx _) = memberSort r ctx

    memberADT r (ContextProc ctx _) = memberADT r ctx

    lookupADT r (ContextProc ctx _) = lookupADT r ctx

    elemsADT (ContextProc ctx _) = elemsADT ctx

    addADTs as (ContextProc ctx vs) = case addADTs as ctx of
                                                        Left e     -> Left e
                                                        Right sctx -> Right $ ContextProc sctx vs

instance FuncContext ContextProc where
    memberFunc r (ContextProc ctx _) = memberFunc r ctx

    lookupFunc r (ContextProc ctx _) = lookupFunc r ctx

    funcSignatures (ContextProc ctx _) = funcSignatures ctx

    elemsFunc (ContextProc ctx _) = elemsFunc ctx

    addFuncs fs (ContextProc ctx ps) = case addFuncs fs ctx of
                                            Left e     -> Left e
                                            Right sctx -> Right $ ContextProc sctx ps

instance ProcContext ContextProc where
    memberProc r ctx = HashMap.member r (procDefs ctx)

    lookupProc r ctx = HashMap.lookup r (procDefs ctx)

    procSignatures ctx = HashMap.keys (procDefs ctx)

    elemsProc ctx = HashMap.elems (procDefs ctx)

    addProcs pds ctx = undefined
        | not $ null nuProcDefs              = Left $ Error ("Non unique process definitions: " ++ show nuProcDefs)
        | not $ null undefinedSorts          = Left $ Error ("List of process signatures with references to undefined sorts: " ++ show undefinedSorts)
        | not $ null undefinedFuncs          = Left $ Error ("List of process signatures with references to undefined funcs: " ++ show undefinedFuncs)
        | not $ null undefinedProcSignatures = Left $ Error ("List of process signatures with references to undefined process signatures: " ++ show undefinedProcSignatures)
        | not $ null undefinedVariables      = Left $ Error ("List of process signatures with undefined variables in their bodies: " ++ show undefinedVariables)
        | otherwise                          = Right $ ctx { _procDefs = definedProcSignatures }
      where
        nuProcDefs :: [ProcDef]
        nuProcDefs = repeatedByProcSignatureIncremental ctx (HashMap.elems (procDefs ctx)) pds

        undefinedSorts :: [(ProcSignature, Set.Set Sort)]
        undefinedSorts = mapMaybe undefinedSort pds

        undefinedSort :: ProcDef -> Maybe (ProcSignature, Set.Set Sort)
        undefinedSort pd = let ps@(ProcSignature _ cs as e) = getProcSignature ctx pd in
                                   case filter (not . memberSort ctx) (concat [concatMap toSorts cs, as, exitSorts e]) of
                                       [] -> Nothing
                                       xs -> Just (ps, Set.fromList xs)

        undefinedVariables :: [(ProcSignature, Set.Set (RefByName VarDef))]
        undefinedVariables = mapMaybe undefinedVariable pds

        undefinedVariable :: ProcDef -> Maybe (ProcSignature, Set.Set (RefByName VarDef))
        undefinedVariable pd = let definedVars :: Set.Set (RefByName VarDef)
                                   definedVars   = Set.fromList (map toRefByName (toList (paramDefs pd)))
                                   freeVars      = freeVars (body pd)
                                   undefinedVars = Set.difference freeVars definedVars
                                in
                                    if Set.null undefinedVars
                                        then Nothing
                                        else Just (getProcSignature ctx pd, undefinedVars)

        definedProcSignatures :: HashMap.Map ProcSignature ProcDef
        definedProcSignatures = HashMap.union (toMapByProcSignature ctx pds) (procDefs ctx)

        undefinedProcSignatures :: [(ProcSignature, Set.Set ProcSignature)]
        undefinedProcSignatures = mapMaybe undefinedProcSignature pds

        undefinedProcSignature :: ProcDef -> Maybe (ProcSignature, Set.Set ProcSignature)
        undefinedProcSignature pd = case findUndefinedProcSignature definedProcSignatures (body pd) of
                                        [] -> Nothing
                                        xs -> Just (getProcSignature ctx pd, Set.fromList xs)
