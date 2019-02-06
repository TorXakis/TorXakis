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
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.BExpr.BExprContext
( -- * BExpr Context
  BExprContext(..)
, MinimalBExprContext (MinimalBExprContext)
--TODO needed?  -- *** Check for usage of undefined process signatures (due to recursive processes)
--, findUndefinedProcSignature
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap           as HashMap
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           TorXakis.BExpr.BExpr
import           TorXakis.ChanDef
import           TorXakis.Error         ( MinError(MinError) )
import           TorXakis.FreeVars
import           TorXakis.Name
import           TorXakis.ProcDef
import           TorXakis.ProcExit
import           TorXakis.ProcSignature
import           TorXakis.Sort          ( Sort, SortContext (..), memberSort )
import           TorXakis.ValExprContext
import           TorXakis.VarContext
import           TorXakis.VarDef
import           TorXakis.VarsDecl

-- | A BExprContext instance contains all definitions to work with behavioural expressions and references thereof
class ValExprContext a => BExprContext a where
    -- | Accessor for Process Definitions
    procDefs :: a -> HashMap.Map ProcSignature ProcDef

    -- | Add process definitions to behavioural expression context.
    --   A value expression context is returned when the following constraints are satisfied:
    --
    --   * The signatures of the processes definitions are unique.
    --
    --   * All references (both Sort, FunctionDefinition, and ProcessDefinition) are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addProcDefs :: a -> [ProcDef] -> Either MinError a


-- | A minimal instance of 'BExprContext'.
data MinimalBExprContext a = MinimalBExprContext { valExprContext :: a
                                                         -- process definitions
                                                 , _procDefs :: HashMap.Map ProcSignature ProcDef
                                                 } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext a => SortContext (MinimalBExprContext a) where
    empty = MinimalBExprContext empty HashMap.empty
    adtDefs ctx    = adtDefs (valExprContext ctx)
    addADTs ctx as = case addADTs (valExprContext ctx) as of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx {valExprContext = vctx}

instance FuncContext a => FuncContext (MinimalBExprContext a) where
    funcDefs ctx    = funcDefs (valExprContext ctx)
    addFuncDefs ctx fs = case addFuncDefs (valExprContext ctx) fs of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx {valExprContext = vctx}

instance ValExprContext a => VarContext (MinimalBExprContext a) where
    varDefs ctx       = varDefs (valExprContext ctx)
    addVars ctx fs = case addVars (valExprContext ctx) fs of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx {valExprContext = vctx}

instance ValExprContext a => ValExprContext (MinimalBExprContext a)

instance ValExprContext a => BExprContext (MinimalBExprContext a) where
    procDefs = _procDefs
    addProcDefs ctx pds
        | not $ null nuProcDefs              = Left $ MinError (T.pack ("Non unique process definitions: " ++ show nuProcDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of process signatures with references to undefined sorts: " ++ show undefinedSorts))
        -- undefined function references already checked in constructors of BExprs
        | not $ null undefinedProcSignatures = Left $ MinError (T.pack ("List of process signatures with references to undefined process signatures: " ++ show undefinedProcSignatures))
        | not $ null undefinedVariables      = Left $ MinError (T.pack ("List of process signatures with undefined variables in their bodies: " ++ show undefinedVariables))
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
                                   usedVars      = freeVars (body pd)
                                   undefinedVars = Set.difference usedVars definedVars
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

-- | Find Undefined Process Signatures in given Behaviour Expression (given the defined Process Signatures)
findUndefinedProcSignature :: HashMap.Map ProcSignature ProcDef -> BExpression -> [ProcSignature]
findUndefinedProcSignature definedProcSignatures = findUndefinedProcSignature'
    where
        findUndefinedProcSignature' :: BExpression -> [ProcSignature]
        findUndefinedProcSignature' = findUndefinedProcSignatureView . TorXakis.BExpr.BExpr.view
        
        findUndefinedProcSignatureView :: BExpressionView -> [ProcSignature]
        findUndefinedProcSignatureView (ActionPref _ _ b) = findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Choice s)         = concatMap findUndefinedProcSignature' (Set.toList s)
        findUndefinedProcSignatureView (Guard _ b)        = findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Parallel _ l)     = concatMap findUndefinedProcSignature' l
        findUndefinedProcSignatureView (Enable a _ b)     = findUndefinedProcSignature' a ++ findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Disable a b)      = findUndefinedProcSignature' a ++ findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Interrupt a b)    = findUndefinedProcSignature' a ++ findUndefinedProcSignature' b
        findUndefinedProcSignatureView (ProcInst p _ _)   = if HashMap.member p definedProcSignatures
                                                              then []
                                                              else [p]
        findUndefinedProcSignatureView (Hide _ b)         = findUndefinedProcSignature' b
