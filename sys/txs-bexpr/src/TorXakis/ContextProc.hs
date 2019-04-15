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
, TorXakis.ContextProc.empty
, fromFuncContext
)
where
import qualified Data.HashMap           as HashMap
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set

import           TorXakis.Chan
import           TorXakis.ContextFunc
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.ProcContext
import           TorXakis.ProcDef
import           TorXakis.ProcSignature
import           TorXakis.Var

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
    -- memberSort   = memberSort . funcContext
    -- since compiler complains:
    --        * Cannot use record selector `funcContext' as a function due to escaped type variables
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

    addProcs pds ctx
        | not $ null nuProcDefs              = Left $ Error ("Non unique process definitions: " ++ show nuProcDefs)
        | not $ null undefinedSorts          = Left $ Error ("List of process signatures with references to undefined sorts: " ++ show undefinedSorts)
        | not $ null undefinedFuncSignatures = Left $ Error ("List of process signatures with references to undefined funcs: " ++ show undefinedFuncSignatures)
        | not $ null undefinedProcSignatures = Left $ Error ("List of process signatures with references to undefined process signatures: " ++ show undefinedProcSignatures)
        | not $ null undefinedVariables      = Left $ Error ("List of process signatures with undefined variables in their bodies: " ++ show undefinedVariables)
        | not $ null undefinedChannels       = Left $ Error ("List of process signatures with undefined channels in their bodies: " ++ show undefinedChannels)
        | otherwise                          = Right $ ctx { procDefs = definedProcs }
      where
        nuProcDefs :: [ProcDef]
        nuProcDefs = repeatedByProcSignatureIncremental ctx (HashMap.elems (procDefs ctx)) pds

        undefinedSorts :: [(ProcSignature, Set.Set Sort)]
        undefinedSorts = undefinedSortsInProcs ctx pds

        undefinedVariables :: [(ProcSignature, Set.Set (RefByName VarDef))]
        undefinedVariables = undefinedVariablesInProcs ctx pds

        undefinedFuncSignatures :: [(ProcSignature, Set.Set FuncSignature)]
        undefinedFuncSignatures = undefinedFuncSignaturesInProcs ctx pds
        
        undefinedChannels :: [(ProcSignature, Set.Set (RefByName ChanDef))]
        undefinedChannels = undefinedChannelsInProcs ctx pds

        definedProcs :: HashMap.Map ProcSignature ProcDef
        definedProcs = HashMap.union (toMapByProcSignature ctx pds) (procDefs ctx)

        undefinedProcSignatures :: [(ProcSignature, Set.Set ProcSignature)]
        undefinedProcSignatures = mapMaybe undefinedProcSignature pds

        undefinedProcSignature :: ProcDef -> Maybe (ProcSignature, Set.Set ProcSignature)
        undefinedProcSignature pd = let definedProcSigs :: Set.Set ProcSignature
                                        definedProcSigs   = Set.fromList (HashMap.keys definedProcs)
                                        usedProcSigs      = usedProcSignatures (body pd)
                                        undefinedProcSigs = Set.difference usedProcSigs definedProcSigs
                                     in
                                        if Set.null undefinedProcSigs
                                            then Nothing
                                            else Just (getProcSignature ctx pd, undefinedProcSigs)
