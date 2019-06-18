{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ContextFunc
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context containing Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module TorXakis.ContextFunc
( -- * Context
  -- ** instance of Func Context
  ContextFunc
, TorXakis.ContextFunc.empty
, TorXakis.ContextFunc.fromSortContext
  -- dependencies, yet part of interface
, module TorXakis.FuncContext
)
where
import qualified Data.HashMap           as HashMap
import           Data.Maybe
import qualified Data.Set               as Set
import qualified Data.Text              as T

import           TorXakis.ContextValExpr.ContextValExpr
import           TorXakis.ContextSort
import           TorXakis.Error
import           TorXakis.FuncContext
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.ValExpr.UnsafeSubst
import           TorXakis.ValExpr.ValExpr

-- | An instance of 'TorXakis.FuncContext'.
data ContextFunc = forall c . SortContext c =>
                            ContextFunc { _sortContext :: c
                                          -- funcSignatures
                                        , funcDefs :: HashMap.Map FuncSignature FuncDef
                                        }

-- | Create ContextFunc from SortContext
fromSortContext :: SortContext c => c -> ContextFunc
fromSortContext sc = ContextFunc sc HashMap.empty

-- | empty
empty :: ContextFunc
empty = TorXakis.ContextFunc.fromSortContext TorXakis.ContextSort.empty

instance SortContext ContextFunc where
    -- Can't use
    -- memberSort   = memberSort . sortContext
    -- since compiler complains:
    --        * Cannot use record selector `sortContext' as a function due to escaped type variables
    --          Probable fix: use pattern-matching syntax instead
    -- For more info see: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=existentialquantification#extension-ExistentialQuantification
    memberSort r (ContextFunc ctx _) = memberSort r ctx

    memberADT r (ContextFunc ctx _) = memberADT r ctx

    lookupADT r (ContextFunc ctx _) = lookupADT r ctx

    elemsADT (ContextFunc ctx _) = elemsADT ctx

    addADTs as (ContextFunc ctx fs) = case addADTs as ctx of
                                           Left e     -> Left e
                                           Right sctx -> Right $ ContextFunc sctx fs

instance FuncContext ContextFunc where
    memberFunc f ctx = HashMap.member f (funcDefs ctx)

    lookupFunc f ctx = HashMap.lookup f (funcDefs ctx)

    funcSignatures ctx = HashMap.keys (funcDefs ctx)

    elemsFunc ctx    = HashMap.elems (funcDefs ctx)

    addFuncs fds ctx
        | not $ null nuFuncDefs              = Left $ Error (T.pack ("Non unique function signatures: " ++ show nuFuncDefs))
        | not $ null undefinedSorts          = Left $ Error (T.pack ("List of function signatures with undefined sorts: " ++ show undefinedSorts))
        | not $ null undefinedVariables      = Left $ Error (T.pack ("List of function signatures with undefined variables in their bodies: " ++ show undefinedVariables))
        | not $ null undefinedFuncSignatures = Left $ Error (T.pack ("List of function signatures with undefined function signatures in their bodies: " ++ show undefinedFuncSignatures))
        | otherwise                          = Right $ newCtx ctx (toMapByFuncSignature ctx fds)
      where
        nuFuncDefs :: [FuncDef]
        nuFuncDefs = repeatedByFuncSignatureIncremental ctx (elemsFunc ctx) fds

        undefinedSorts :: [(FuncSignature, Set.Set Sort)]
        undefinedSorts = undefinedSortsInFuncs ctx fds
        
        undefinedVariables :: [(FuncSignature, Set.Set (RefByName VarDef))]
        undefinedVariables = undefinedVariablesInFuncs ctx fds
        
        definedFuncSignatures :: Set.Set FuncSignature
        definedFuncSignatures = Set.fromList (funcSignatures ctx ++ map (getFuncSignature ctx) fds)

        undefinedFuncSignatures :: [(FuncSignature, Set.Set FuncSignature)]
        undefinedFuncSignatures = mapMaybe maybeUndefinedFuncSignatures fds

        maybeUndefinedFuncSignatures :: FuncDef -> Maybe (FuncSignature, Set.Set FuncSignature)
        maybeUndefinedFuncSignatures fd = let funcSignaturesUsed = usedFuncSignatures (body fd)
                                              funcSignaturesUndefined = funcSignaturesUsed `Set.difference` definedFuncSignatures
                                            in
                                               if Set.null funcSignaturesUndefined
                                                  then Nothing
                                                  else Just (getFuncSignature ctx fd, funcSignaturesUndefined)

        newCtx :: ContextFunc -> HashMap.Map FuncSignature FuncDef -> ContextFunc
        newCtx ctx' mfs = let updateCtx = ctx'{ funcDefs = HashMap.union (funcDefs ctx') mfs }
                              (lm, rm)  = HashMap.mapEither (newFuncDef updateCtx) mfs in
                                if HashMap.null lm
                                    then let (lc, rc) = HashMap.partition isConstBody rm in
                                            if HashMap.null lc
                                                then ctx'{ funcDefs = HashMap.union (funcDefs ctx') rc }
                                                else newCtx (ctx'{ funcDefs = HashMap.union (funcDefs ctx') lc }) rc
                                    else error ("All check passed, yet errors occurred\n" ++ show (HashMap.elems lm))

        newFuncDef :: ContextFunc -> FuncDef -> Either Error FuncDef
        newFuncDef updateCtx fd = let nm = TorXakis.FuncDef.funcName fd
                                      ps = paramDefs fd
                                      bd = body fd in
                                        unsafeSubst updateCtx HashMap.empty bd >>= mkFuncDef ctx nm ps

        isConstBody :: FuncDef -> Bool
        isConstBody fd = case view (body fd) of
                                Vconst {} -> True
                                _         -> False
