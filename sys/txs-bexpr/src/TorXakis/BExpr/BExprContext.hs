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
, MinimalBExprContext (..)
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
import           TorXakis.BExpr.ExitKind
import           TorXakis.ChanDef
import           TorXakis.Error         ( MinError(MinError) )
import           TorXakis.Sort          ( Sort, SortContext (..), elemSort )
import           TorXakis.ValExpr
import           TorXakis.VarDef        ( MinimalVarDef )
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
    empty = MinimalBExprContext (empty :: MinimalValExprContext MinimalVarDef) HashMap.empty
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
        undefinedSort pd = let ps@(ProcSignature _ cs as e) = getProcSignature pd in
                            case filter (not . elemSort ctx) (concat [concatMap toSorts cs, as, exitSorts e]) of
                                [] -> Nothing
                                xs -> Just (ps, Set.fromList xs)

        definedProcSignatures :: HashMap.Map ProcSignature ProcDef
        definedProcSignatures = HashMap.union (toMapByProcSignature pds) (_procDefs ctx)

        undefinedProcSignatures :: [(ProcSignature, Set.Set ProcSignature)]
        undefinedProcSignatures = mapMaybe undefinedProcSignature pds

        undefinedProcSignature :: ProcDef -> Maybe (ProcSignature, Set.Set ProcSignature)
        undefinedProcSignature pd = case findUndefinedProcSignature definedProcSignatures (body pd) of
                                        [] -> Nothing
                                        xs -> Just (getProcSignature pd, Set.fromList xs)

-- | Find Undefined Process Signatures in given Behaviour Expression (given the defined Process Signatures)
findUndefinedProcSignature :: HashMap.Map ProcSignature ProcDef -> BExpr -> [ProcSignature]
findUndefinedProcSignature definedProcSignatures = findUndefinedProcSignature'
    where
        findUndefinedProcSignature' :: BExpr -> [ProcSignature]
        findUndefinedProcSignature' = findUndefinedProcSignatureView . TorXakis.BExpr.BExpr.view
        
        findUndefinedProcSignatureView :: BExprView -> [ProcSignature]
        findUndefinedProcSignatureView (ActionPref _ b) = findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Guard _ b)      = findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Choice s)       = concatMap findUndefinedProcSignature' (Set.toList s)
        findUndefinedProcSignatureView (Parallel _ l)   = concatMap findUndefinedProcSignature' l
        findUndefinedProcSignatureView (Enable a _ b)   = findUndefinedProcSignature' a ++ findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Disable a b)    = findUndefinedProcSignature' a ++ findUndefinedProcSignature' b
        findUndefinedProcSignatureView (Interrupt a b)  = findUndefinedProcSignature' a ++ findUndefinedProcSignature' b
        findUndefinedProcSignatureView (ProcInst p _ _) = if HashMap.member p definedProcSignatures
                                                            then []
                                                            else [p]
        findUndefinedProcSignatureView (Hide _ b)      = findUndefinedProcSignature' b
