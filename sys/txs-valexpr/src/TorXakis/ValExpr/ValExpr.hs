{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ValExpr.ValExpr
( -- * Value Expression
  ValExpressionView (..)
, ValExpression (..)
, ValExprView
, ValExpr
  -- * Evaluate
, eval
)
where
import           Control.DeepSeq    (NFData)
import           Data.Data          (Data)
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import           GHC.Generics       (Generic)


import           TorXakis.FreeVars
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Value
import           TorXakis.VarDef

-- | ValExpressionView: the public view of value expression 'ValExpression'
data ValExpressionView v = Vconst    Value
                   | Vvar      v                                                                        -- Sort is stored to prevent lookup in context
                   -- generic
                   | Vequal    (ValExpression v)
                               (ValExpression v)
                   | Vite      (ValExpression v)
                               (ValExpression v)
                               (ValExpression v)
                   | Vfunc     FuncSignature [ValExpression v]
                   | Vpredef   FuncSignature [ValExpression v]
                   -- Boolean
                   | Vnot      (ValExpression v)
                   | Vand      (Set.Set (ValExpression v))
                   -- Int
                   | Vdivide   (ValExpression v)
                               (ValExpression v)
                   | Vmodulo   (ValExpression v)
                               (ValExpression v)
                   | Vsum      (Map.Map (ValExpression v) Integer)
                   | Vproduct  (Map.Map (ValExpression v) Integer)
                   | Vgez      (ValExpression v)
                   -- String
                   | Vlength   (ValExpression v)
                   | Vat       (ValExpression v)
                               (ValExpression v)
                   | Vconcat   [ValExpression v]
                   -- Regex
                   | Vstrinre  (ValExpression v)
                               (ValExpression v)
                   -- ADT
                   | Vcstr     (RefByName ADTDef) (RefByName ConstructorDef) [ValExpression v]
                   | Viscstr   (RefByName ADTDef) (RefByName ConstructorDef) (ValExpression v)
                   | Vaccess   (RefByName ADTDef) (RefByName ConstructorDef) Sort Int (ValExpression v)       -- Sort is stored to prevent lookup in context
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | ValExpression: value expression
--
-- 1. User can't directly construct ValExpression (such that invariants will always hold)
--
-- 2. User can still pattern match on ValExpression using 'ValExpressionView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype ValExpression v = ValExpression { -- | View on value expression.
                                          view :: ValExpressionView v
                                        }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

  -- | type synonym ValExpr
type ValExpr = ValExpression MinimalVarDef

-- | type synonym ValExprView
type ValExprView = ValExpressionView MinimalVarDef

-- | Evaluate the provided value expression.
-- Either the Right Constant Value is returned or a (Left) error message.
eval :: Show v => ValExpression v -> Either String Value
eval = evalView . view

evalView :: Show v => ValExpressionView v -> Either String Value
evalView (Vconst v) = Right v
evalView x          = Left $ "Value Expression is not a constant value " ++ show x

-- | SortOf instance
instance VarDef v => HasSort (ValExpression v) where
  getSort = getSort . view

instance VarDef v => HasSort (ValExpressionView v) where
    getSort (Vconst val)                                   = getSort val
    getSort (Vvar v)                                       = getSort v
    getSort  Vequal { }                                    = SortBool
    getSort (Vite _cond vexp1 _vexp2)                      = getSort vexp1
    getSort  Vnot { }                                      = SortBool
    getSort  Vand { }                                      = SortBool
    getSort  Vdivide { }                                   = SortInt
    getSort  Vmodulo { }                                   = SortInt
    getSort  Vsum { }                                      = SortInt
    getSort  Vproduct { }                                  = SortInt
    getSort  Vgez { }                                      = SortBool
    getSort  Vlength { }                                   = SortInt
    getSort  Vat { }                                       = SortString
    getSort  Vconcat { }                                   = SortString
    getSort  Vstrinre { }                                  = SortBool
    getSort (Vcstr a _c _vexps)                            = SortADT a
    getSort  Viscstr { }                                   = SortBool
    getSort (Vaccess _a _c s _p _vexps)                    = s
    getSort (Vfunc fs _vexps)                              = returnSort fs
    getSort (Vpredef fs _vexps)                            = returnSort fs

instance VarDef v => FreeVars ValExpression v where
    freeVars = freeVars . view

instance VarDef v => FreeVars ValExpressionView v where
    freeVars  Vconst{}             = Set.empty
    freeVars (Vvar v)              = Set.singleton v
    freeVars (Vequal v1 v2)        = Set.unions $ map freeVars [v1, v2]
    freeVars (Vite c t f)          = Set.unions $ map freeVars [c, t, f]
    freeVars (Vfunc _ as)          = Set.unions $ map freeVars as
    freeVars (Vpredef _ as)        = Set.unions $ map freeVars as
    freeVars (Vnot v)              = freeVars v
    freeVars (Vand s)              = Set.unions $ map freeVars (Set.toList s)
    freeVars (Vdivide t n)         = Set.unions $ map freeVars [t, n]
    freeVars (Vmodulo t n)         = Set.unions $ map freeVars [t, n]
    freeVars (Vsum m)              = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vproduct m)          = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vgez v)              = freeVars v
    freeVars (Vlength v)           = freeVars v
    freeVars (Vat s p)             = Set.unions $ map freeVars [s, p]
    freeVars (Vconcat vs)          = Set.unions $ map freeVars vs
    freeVars (Vstrinre s r)        = Set.unions $ map freeVars [s, r]
    freeVars (Vcstr _ _ vs)        = Set.unions $ map freeVars vs
    freeVars (Viscstr _ _ v)       = freeVars v
    freeVars (Vaccess _ _ _ _ v)   = freeVars v

