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
  ValExprContext (..)
, MinimalValExprContext(MinimalValExprContext)
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap           as HashMap
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import qualified Data.Text              as T
import           GHC.Generics           (Generic)

import           TorXakis.Error         ( MinError(MinError) )
import           TorXakis.Name          ( RefByName, toMapByName, repeatedByName )
import           TorXakis.Sort          ( Sort, SortContext (..), MinimalSortContext (..), elemSort, getSort )
import           TorXakis.ValExpr
import           TorXakis.VarDef        ( VarDef, MinimalVarDef )
import           TorXakis.ProcDef
import           TorXakis.ProcSignature



-- | A BExprContext instance contains all definitions to work with behavioural expressions and references thereof
class ValExprContext a v => BExprContext a v where
    -- | Accessor for Process Definitions
    procDefs :: a v -> HashMap.Map ProcSignature ProcDef

    -- | Add process definitions to behavioural expression context.
    --   A value expression context is returned when the following constraints are satisfied:
    --
    --   * The signatures of the processes definitions are unique.
    --
    --   * All references (both Sort, FunctionDefinition, and ProcessDefinition) are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addProcDefs :: a v -> [ProcDef] -> Either MinError (a v)


-- | A minimal instance of 'ValExprContext'.
data MinimalBExprContext v = MinimalBExprContext { valExprContext :: MinimalValExprContext v
                                                         -- process definitions
                                                 , _procDefs :: HashMap.Map ProcSignature ProcDef
                                                 } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext (MinimalBExprContext MinimalVarDef) where
    empty = MinimalBExprContext (empty::MinimalValExprContext) HashMap.empty
    adtDefs ctx    = adtDefs (valExprContext ctx)
    addAdtDefs ctx as = case addAdtDefs (valExprContext ctx) as of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx {valExprContext = vctx} 

instance ValExprContext MinimalBExprContext MinimalVarDef where
    varDefs ctx    = varDefs (valExprContext ctx)
    addVarDefs ctx vs = case addVarDefs (valExprContext ctx) vs of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx {valExprContext = vctx} 
    funcDefs ctx    = funcDefs (valExprContext ctx)
    addFuncDefs ctx fs = case addFuncDefs (valExprContext ctx) fs of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx {valExprContext = vctx} 

instance BExprContext MinimalBExprContext MinimalVarDef where
    procDefs = _procDefs
    addProcDefs ctx pds
        | not $ null nuProcDefs              = Left $ MinError (T.pack ("Non unique process definitions: " ++ show nuProcDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of process signatures with references to undefined sorts: " ++ show undefinedSorts))
        -- undefined function references already checked in constructors of BExprs
        | not $ null undefinedProcSignatures = Left $ MinError (T.pack ("List of process signatures with references to undefined process signatures: " ++ show undefinedProcSignatures))
        | otherwise                          = Right $ ctx { _procDefs = definedProcSignatures }
      where
        nuProcDefs :: [ProcDef]
        nuProcDefs = repeatedByProcSignatureIncremental (HashMap.elems (procDefs ctx)) pds

        undefinedSorts :: [(ProcSignature, Set.Set Sort)]
        undefinedSorts = mapMaybe undefinedSort pds

        undefinedSort :: HasProcSignature a => a -> Maybe (ProcSignature, Set.Set Sort)
        undefinedSort pd = let ps@(ProcSignature ...) = getProcSignature pd in
                            case filter (not . elemSort ctx) (...) of
                                [] -> Nothing
                                xs -> Just (ps, Set.fromList xs)

        definedProcSignatures :: HashMap.Map ProcSignature ProcDef
        definedProcSignatures = HashMap.union (toMapByProcSignature pds) (_procDefs ctx)

        undefinedProcSignatures :: [(ProcSignature, Set.Set ProcSignature)]
        undefinedProcSignatures = mapMaybe undefinedProcSignature pds

        undefinedProcSignature :: ProcDef MinimalVarDef -> Maybe (ProcSignature, Set.Set ProcSignature)
        undefinedProcSignature pd = case findUndefinedProcSignature (body pd) of
                                        [] -> Nothing
                                        xs -> Just (getProcSignature pd, Set.fromList xs)

        findUndefinedProcSignature :: BExpr -> [ProcSignature]
        findUndefinedProcSignature = findUndefinedProcSignature' . view

        findUndefinedProcSignature' :: BExprView -> [ProcSignature]
        